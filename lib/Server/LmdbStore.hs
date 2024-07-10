-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE StrictData       #-}
{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Server.LmdbStore
    ( LmdbStore(..)
    , Cushion(..)
    --
    , openDatastore
    , hasSnapshot
    , loadLogBatches
    --
    , writeLogBatch
    , writeMachineSnapshot
    --
    , loadPinByHash
    , internPin
    )
where

import Database.LMDB.Raw
import Fan.Prof
import Fan.Types
import PlunderPrelude               hiding (log)
import Server.Convert               ()
import Server.Debug
import Server.LmdbStore.MdbValue
import Server.LmdbStore.PinMetadata
import Server.Types.Logging
import System.IO.Posix.MMap
import UnliftIO.IO.File

import Control.Monad.Trans.State (StateT, execStateT, get, modify')
import Data.Acquire              (Acquire, ReleaseType(..), mkAcquire,
                                  mkAcquireType, with)
import Data.HashMap.Strict       ()
import Fan.Convert               (fromNoun, toNoun)
import Foreign.Marshal.Alloc     (allocaBytes)
import Foreign.Ptr               (Ptr, nullPtr)
import Foreign.Storable          (Storable(..), poke, sizeOf)
import Hash256                   (hashToByteString, encodeBtc)
import System.Directory          (createDirectoryIfMissing)
import System.FilePath           (takeDirectory)

import Fan.Seed (savePin')

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as VS
import qualified Fan                  as F
import qualified Fan.Seed             as Seed


-- -----------------------------------------------------------------------
-- Low level interface
-- -----------------------------------------------------------------------

data LmdbStore = LmdbStore
    { dir           :: FilePath
    , env           :: MDB_env

    -- TODO: Was MachineLogTable / machineEventlogTables
    , logTable      :: MDB_dbi
    , snapshotTable :: MDB_dbi
    , numBatches    :: TVar Word64

    -- | A mapping of noun hash to serialized noun representation.
    , pinCushion    :: MDB_dbi

    -- | All known pins. This is loaded from `pinCushion` at startup and is
    -- meant to be kept in sync.
    , knownPins     :: IORef (HashMap Hash256 PinState)
    }


-- | Each loaded pin has an entry about its committal state.
--
-- TODO: Is there a way to use `Weak` to keep a weak reference to the Pin? We
-- could drop the Cushion cache entirely if we could just keep expirable pin
-- references here.
data PinState
    -- Pin not yet committed to LMDB
    = PIN_PENDING ByteString Pend
    -- Pin record durably committed to the LMDB `pinCushion` table.
    -- The bytestring is a memory-mapped view into the file.
    | PIN_COMMITTED Word64

data Pend = PEND {
    written :: MVar (),          -- ^ Set once written to disk.
    fsynced :: MVar ()           -- ^ Set once written data is durable on disk.
    }

data DecodeCtx
    = DECODE_PIN Hash256
    | DECODE_BATCH Word64
  deriving Show

data StoreExn
    = BadWriteLogBatch BatchNum
    | BadWriteSnapshot BatchNum
    | BadPinWrite Word64 Hash256
    | BadPinMissing Hash256 [Hash256]
    | BadBlob DecodeCtx Seed.LoadErr
    | BadDependency ByteString
    | EmptyLogTable
    | EmptySnapshotTable
  deriving Show

instance Exception StoreExn where

makeFieldLabelsNoPrefix ''LmdbStore

-- TODO Keys have uniform distribution.  Use something faster than Data.Map.
newtype Cushion = CUSHION { tab :: IORef (HashMap Hash256 F.Pin) }


-- Utils -----------------------------------------------------------------------

withNullKVPtrs :: (Ptr MDB_val -> Ptr MDB_val -> IO a) -> IO a
withNullKVPtrs = withKVPtrs (MDB_val 0 nullPtr) (MDB_val 0 nullPtr)


-- Environment and Tables ------------------------------------------------------

openEnv :: MonadIO m => FilePath -> m MDB_env
openEnv dir = liftIO $ do
    env <- mdb_env_create
    mdb_env_set_maxdbs env 1024 -- TODO: Adjust this dynamically?
    mdb_env_set_mapsize env (1024 ^ (4::Int))
    mdb_env_open env dir []
    pure env

openTables :: MDB_txn -> IO (MDB_dbi, MDB_dbi, MDB_dbi)
openTables txn =
    (,,) <$> mdb_dbi_open txn (Just "LOG") [MDB_CREATE, MDB_INTEGERKEY]
         <*> mdb_dbi_open txn (Just "SNAPSHOT") [MDB_CREATE, MDB_INTEGERKEY]
         <*> mdb_dbi_open txn (Just "NOUNS") [MDB_CREATE, MDB_INTEGERKEY]

open :: FilePath -> IO LmdbStore
open dir = do
    createDirectoryIfMissing True dir
    env <- openEnv dir
    with (writeTxn env) $ \txn -> do
        (logTable, snapshotTable, pinCushion) <- openTables txn
        numBatches <- readNumBatches txn logTable >>= newTVarIO
        knownPinData <- mapFromList <$> readPins txn pinCushion
        knownPins <- newIORef knownPinData
        pure $ LmdbStore{..}
  where
    readPins :: MDB_txn -> MDB_dbi -> IO [(Hash256, PinState)]
    readPins txn pinCushion =
      with (cursor txn pinCushion) \cur ->
      forAllKV cur \pKey pVal -> do
        pinId <- peekMdbVal @Word64 pKey
        meta  <- peekMdbVal @PinMetadata pVal
        -- print ("ReadPin"::Text, meta)
        pure (meta.hash, PIN_COMMITTED pinId)

close :: LmdbStore -> IO ()
close db = do
    mdb_dbi_close db.env db.logTable
    mdb_dbi_close db.env db.snapshotTable
    mdb_dbi_close db.env db.pinCushion
    mdb_env_sync_flush db.env
    mdb_env_close db.env

-- -----------------------------------------------------------------------

readNumBatches :: MDB_txn -> MDB_dbi -> IO Word64
readNumBatches txn logTable =
    with (cursor txn logTable) $ \cur ->
        withNullKVPtrs \k v -> do
            mdb_cursor_get MDB_LAST cur k v >>= \case
                False -> pure 0
                True  -> peekMdbVal @Word64 k

-- True if a snapshot has been written. Used during boot to detect whether a
-- directory has been booted previously so you don't overwrite.
hasSnapshot :: LmdbStore -> IO Bool
hasSnapshot store = do
    with (readTxn store.env) $ \txn -> do
        with (cursor txn store.snapshotTable) $ \cur -> do
            withNullKVPtrs \k v -> do
                mdb_cursor_get MDB_FIRST cur k v

loadLogBatches
    :: forall a
     . ReplayFrom
    -> ((CogState, BatchNum) -> a)
    -> (a -> LogBatch -> IO a)
    -> LmdbStore
    -> Cushion
    -> IO a
loadLogBatches replayFrom mkInitial fun lmdbStore cache = do
    with (readTxn lmdbStore.env) $
      (loadBatches lmdbStore.logTable lmdbStore.snapshotTable)
  where
    loadBatches logTable snapshotTable txn = do
      (ssBatch, initial) <- findSnapshot snapshotTable txn
      readRangeStarting logTable txn ssBatch initial

    readRangeStarting log txn ssRaw initial = do
      let firstLogBatch = ssRaw + 1
      with (cursor txn log) $ \cur -> do
        foldlRange cur firstLogBatch initial \prev k v -> do
          key <- peekMdbVal @Word64 k
          vBS <- peekMdbVal @ByteString v
          val <- loadNoun lmdbStore cache (DECODE_BATCH key) vBS
          case fromNoun val of
            Nothing -> error "Couldn't read noun"
            Just lb -> fun prev lb

    findSnapshot :: MDB_dbi -> MDB_txn -> IO (Word64, a)
    findSnapshot snapshotTbl txn = do
      with (cursor txn snapshotTbl) $ \cur -> do
        withNullKVPtrs \k v -> do
          let op = case replayFrom of
                EarliestSnapshot -> MDB_FIRST
                LatestSnapshot   -> MDB_LAST
          found <- mdb_cursor_get op cur k v
          unless found $ throwIO EmptySnapshotTable
          key <- peekMdbVal @Word64 k
          vBS <- peekMdbVal @Hash256 v

          pin   <- loadPinByHash lmdbStore cache [] vBS
          case fromNoun @CogState pin.item of
            Nothing -> error "Couldn't read snapshot"
            Just ss ->
              pure (key, mkInitial (ss, BatchNum $ fromIntegral key))

writeLogBatch :: Debug => LmdbStore -> LogBatch -> IO ()
writeLogBatch lmdbStore lb = do
    withTraceResultArgs "writeLogBatch" "log" do
      (pinz, lbPin, lbHead, lbBody) <-
        withSimpleTracingEvent "jar+hash" "log" $ do
          Seed.withContext \ctx -> do
            pin <- F.mkPin' (toNoun lb)
            (!pinz, lbHead, lbBody) <- savePin' ctx pin
            void (evaluate pin.hash)
            pure (pinz, pin, lbHead, lbBody)
      numBatches <- atomically (readTVar lmdbStore.numBatches)
      let curBatch = numBatches + 1
      withSimpleTracingEvent "writeIt" "log" $
        writeIt curBatch lmdbStore.logTable lbPin.hash lbHead lbBody pinz
      atomically $ modifyTVar' lmdbStore.numBatches (+1)
      let args = mapFromList [
            ("batchNum", Left $ fromIntegral $ unBatchNum $ batchNum lb),
            ("receiptCount", Left $ fromIntegral $ length $ executed lb)
            ]
      pure (args, ())
  where
    logFlags = compileWriteFlags [MDB_NOOVERWRITE, MDB_APPEND]

    -- "it" being the current batch.
    writeIt curBatch logTable _lbHash lbHead lbBody pinz = do
        when (curBatch /= (fromIntegral $ unBatchNum lb.batchNum)) $
            error ( "Database/Runtime batch number mismatch: database expects "
                 ++ show curBatch ++ " but received "
                 ++ show (unBatchNum lb.batchNum)
                  )

        topRep <- withSimpleTracingEvent "serializeRep" "log" $
          evaluate (lbHead <> lbBody)

        pinBatch <- gatherWriteBatch lmdbStore (toList pinz)

        writePinsToDisk lmdbStore pinBatch

        withWriteAsyncCommitPins lmdbStore (fst <$> toList pinBatch) $ \txn -> do
          writeKV logFlags txn logTable
              (curBatch::Word64)
              topRep
              (BadWriteLogBatch lb.batchNum)

writeMachineSnapshot :: Debug => LmdbStore -> BatchNum
                     -> CogState
                     -> IO ()
writeMachineSnapshot lmdbStore (BatchNum bn) f = do
    let p = coerceIntoPin $ toNoun f
        logFlags = compileWriteFlags [MDB_NOOVERWRITE, MDB_APPEND]

    pinBatch <- gatherWriteBatch lmdbStore [p]

    writePinsToDisk lmdbStore pinBatch

    withWriteAsyncCommitPins lmdbStore (fst <$> toList pinBatch) $ \txn -> do
        -- Don't write duplicate snapshots. This can happen when you spin a
        -- machine, and then shut down the machine before enough work is
        -- done to naturally write a logbatch.
        let key = fromIntegral bn :: Word64
        writeKVNoOverwrite logFlags txn lmdbStore.snapshotTable key p.hash
                           (BadWriteSnapshot (BatchNum bn))

gatherWriteBatch
    :: LmdbStore
    -> [F.Pin]
    -> IO (HashMap Hash256 (PinMetadata_ (Vector Hash256), ByteString))
gatherWriteBatch store topPins = do
    withSimpleTracingEvent "gatherWriteBatch" "log" $ do
        st <- readIORef store.knownPins
        Seed.withContext \ctx ->
            flip execStateT mempty do
                for_ topPins (gatherWriteBatchWorker ctx st)

type GatherSt = HashMap Hash256 (PinMetadata_ (Vector Hash256), ByteString)

gatherWriteBatchWorker
    :: Seed.Ctx
    -> HashMap Hash256 PinState
    -> F.Pin
    -> StateT GatherSt IO ()
gatherWriteBatchWorker ctx dbState pin = do
    st <- get
    case (lookup pin.hash st, lookup pin.hash dbState) of
        (Just{},  _                     ) -> pure ()
        (Nothing, Nothing               ) -> weSerialize
        (Nothing, Just (PIN_PENDING b _)) -> weWait b
        (Nothing, Just PIN_COMMITTED{}  ) -> pure ()
  where
    loop = gatherWriteBatchWorker ctx dbState

    -- Someone else is writing.  When we go to write this we will actually
    -- just wait.
    weWait :: ByteString -> StateT GatherSt IO ()
    weWait v =
        modify' (insertMap pin.hash (mkPinMeta pin.hash v edges, v))
      where
        edges = pin.refs <&> (.hash)

    weSerialize :: StateT GatherSt IO ()
    weSerialize = do
        (refs, hed, bod) <- liftIO $ withSimpleTracingEvent "Save" "log" do
                                savePin' ctx pin
        traverse_ loop refs
        let haz = pin.hash
        let bar = hashToByteString haz <> hed <> bod
        let hashes = refs <&> (.hash)
        modify' (insertMap haz (mkPinMeta haz bar hashes, bar))

    mkPinMeta
        :: Hash256
        -> ByteString
        -> Vector Hash256
        -> PinMetadata_ (Vector Hash256)
    mkPinMeta h b r = PIN_META h (fromIntegral $ length b) r

-- | Blocks until the pin is in a state where it can be safely loaded from
-- disk.
--
-- - If the pin doesn't exist on disk at all, return False.
--
-- - If the pin has been fully committed, return True.
--
-- - If the pin has been written to disk but is not yet committed, then block
--   until it has been fsynced to disk and return True.
waitPinLoadable :: LmdbStore -> Hash256 -> IO Bool
waitPinLoadable lmdbStore pinHash = do
  state <- readIORef lmdbStore.knownPins
  case lookup pinHash state of
    Nothing                            -> pure False
    Just (PIN_PENDING _ PEND{written}) -> readMVar written >> pure True
    Just PIN_COMMITTED{}               -> pure True

-- TODO: Don't use so many open files!
-- | Writes pins to individual mmappable files.
writePinsToDisk :: Debug
                => LmdbStore
                -> HashMap Hash256 (PinMetadata_ a, ByteString)
                -> IO ()
writePinsToDisk lmdbStore pinBatch = do
  withSimpleTracingEvent "writePinsToDisk" "log" $ do
    forM_ (mapToList pinBatch) $ \(k,(_,bs)) -> do
      writePinAndStartFsync lmdbStore (pinNameToPath lmdbStore.dir k) k bs

-- | Writes references to pins we've successfully written to disk to
-- lmdb. Existence of a pin is never checked against the file directory, only
-- against the durable lmdb database.
recordAlreadySyncedPinsInLMDB
    :: HashMap Hash256 PinState
    -> (MDB_WriteFlags, MDB_txn, MDB_dbi)
    -> [PinMetadata_ (Vector Hash256)]
    -> IO (Map Word64 PinMetadata)
recordAlreadySyncedPinsInLMDB dbState (logFlags, txn, pinCushion) pins =
    withSimpleTracingEvent "recordAlreadySyncedPinsInLMDB" "log" do

    bt <- assignRowIds dbState (txn, pinCushion) pins

    forM_ (mapToList bt) $ \(pinId, pinData) -> do
        let err = BadPinWrite pinId pinData.hash
        -- pPrint ("WritePin"::Text, pinId, pinData)
        writeKV logFlags txn pinCushion pinId pinData err
        -- `LmdbStore` is responsible for not inserting the same pin
        -- twice.

    pure bt

assignRowIds
    :: HashMap Hash256 PinState
    -> (MDB_txn, MDB_dbi)
    -> [PinMetadata_ (Vector Hash256)]
    -> IO (Map Word64 PinMetadata)
assignRowIds dbState ctx pins = do
    nextPinId <- getNextAvailablePinId ctx

    -- Associate a new rowId with each pin hash
    let batch = mapFromList (zip [nextPinId..] pins)

    -- Given a hash, find the rowId that referenced it.
    let hashToRowId = mapFromList (zip (pins <&> (.hash)) [nextPinId..])
                        :: HashMap Hash256 Word64

    let findRowId :: Hash256 -> Word64
        findRowId hax =
            case (lookup hax hashToRowId, lookup hax dbState) of
                (Just rowId, ~_)               -> rowId
                (_, Just(PIN_COMMITTED rowId)) -> rowId
                _                              -> error "BadBatch: Unknown Hash"

    pure (batch <&> \m -> m {edges=VS.convert (findRowId <$> m.edges)})

getNextAvailablePinId :: (MDB_txn, MDB_dbi) -> IO Word64
getNextAvailablePinId (txn, pinCushion) =
    fmap (maybe 0 succ) $
    with (cursor txn pinCushion) \cur ->
    withNullKVPtrs \k v -> do
        mdb_cursor_get MDB_LAST cur k v >>= \case
            False -> pure Nothing
            True  -> Just <$> peekMdbVal @Word64 k

-- | Given a pin, block to intern everything about the pin to disk and return
-- the pin loaded back from disk so that it's backed by a memory mapped
-- structure.
internPin :: Debug => LmdbStore -> F.Pin -> Cushion -> IO F.Pin
internPin lmdbStore pin cushion = do
  withSimpleTracingEvent "internPin" "log" $ do
    pinBatch <- gatherWriteBatch lmdbStore [pin]

    writePinsToDisk lmdbStore pinBatch

    -- We've written the pins to disk, but we have no durability guarantees
    -- until we've waited for the fsync to complete and the pin is in the lmdb
    -- table.

    -- TODO: Avoid waiting for sync by just doing the mmap immediatly
    -- when we write.  Store it in the pin state.

    loadPinByHash lmdbStore cushion [] pin.hash

data PinWriteResponsibility
  = WritePin
  | WaitForPinWrite (MVar ())
  | AlreadyCommitted

-- | Ensures a pin is written to disk, with it being fsynced for durability
-- sometime in the future in a monitorable state.
writePinAndStartFsync
    :: Debug
    => LmdbStore -> FilePath -> Hash256 -> ByteString
    -> IO ()
writePinAndStartFsync lmdbStore filePath pHash pFullBlob = do
  newWritten <- newEmptyMVar
  newFsynced <- newEmptyMVar

  doWrite <- atomicModifyIORef lmdbStore.knownPins \m ->
    case lookup pHash m of
      Nothing -> let pinState = PIN_PENDING pFullBlob
                                    $ PEND { written = newWritten
                                           , fsynced = newFsynced
                                           }
                 in (insertMap pHash pinState m, WritePin)
      Just (PIN_PENDING _ PEND{written}) -> (m, WaitForPinWrite written)
      Just PIN_COMMITTED{}               -> (m, AlreadyCommitted)

  case doWrite of
    WritePin -> do
      createDirectoryIfMissing True (takeDirectory filePath)
      BS.writeFile filePath pFullBlob
      putMVar newWritten ()
      _ <- async $ fsyncAsync filePath newFsynced
      pure ()

    WaitForPinWrite waiter -> do
      -- Some other thread is already handling writing this pin to disk and
      -- will be responsible for kicking off the fsync process.
      readMVar waiter

    AlreadyCommitted -> pure ()

  where
    fsyncAsync path completeVar = do
      -- Do all the durability stuff. This will open file descriptors for the
      -- file and the containing directory, do nothing with them, and then
      -- fsyncing both file descriptors.
      withBinaryFileDurable path ReadWriteMode (\_ -> pure ())

      putMVar completeVar ()

-- | Ensures that every pin listed exists and has been fsynced. Called before
-- we commit data to the lmdb database.
ensurePinsFsynced :: LmdbStore -> [PinMetadata_ a] -> IO ()
ensurePinsFsynced lmdbStore pins =
  withSimpleTracingEvent "ensurePinsFsynced" "log" (loop pins)
  where
    loop [] = pure ()
    loop (x:xs) = do
      knownPins <- readIORef lmdbStore.knownPins
      case lookup x.hash knownPins of
        Nothing -> error "Missing pin task in ensurePinsFsynced"
        Just (PIN_PENDING _ PEND{fsynced}) -> readMVar fsynced
        Just PIN_COMMITTED{}               -> pure ()
      loop xs

coerceIntoPin :: F.Fan -> F.Pin
coerceIntoPin (F.PIN p) = p
coerceIntoPin fan       = coerceIntoPin (F.mkPin fan)

-- | Given a name, find a pin in the pin cushion.
loadPinByHash :: LmdbStore -> Cushion -> [Hash256] -> Hash256
              -> IO F.Pin
loadPinByHash lmdbStore cache topStack =
    \key -> loop topStack key
  where
    loop :: [Hash256] -> Hash256 -> IO F.Pin
    loop !stack pinHash = do
        (lookup pinHash <$> readIORef cache.tab) >>= \case
            Nothing -> do
                res <- go stack pinHash
                modifyIORef' cache.tab (insertMap pinHash res)
                pure res
            Just pin -> do
               pure pin

    go :: [Hash256] -> Hash256 -> IO F.Pin
    go !stack pinHash = do
        waitPinLoadable lmdbStore pinHash >>= \case
            False -> do
                throwIO $ BadPinMissing pinHash stack
            True -> do
                blob <- unsafeMMapFile (pinNameToPath lmdbStore.dir pinHash)
                let stk  = (pinHash : stack)
                let deco = DECODE_PIN pinHash
                (pinz, pin) <- decodeBlob (loop stk) deco (drop 32 blob)
                F.loadPinFromBlob pinz pinHash pin -- HACK

decodeBlob
    :: (Hash256 -> IO F.Pin)
    -> DecodeCtx
    -> ByteString
    -> IO (Vector F.Pin, Fan)
decodeBlob loadRefr key blob = do
    withSimpleTracingEvent "decodeBlob" "log" do
        (hed, bod) <- orExn (Seed.splitBlob blob)
        refs       <- orExn (Seed.loadHead hed)
        pinz       <- traverse loadRefr refs
        valu       <- withSimpleTracingEvent "loadBody" "log" do
                          orExn (Seed.loadBody pinz bod)
        pure (pinz, valu)
  where
    orExn :: ∀a. Either Seed.LoadErr a -> IO a
    orExn x = either (throwIO . BadBlob key) pure x

-- | Given a bytestring representing the jar-entry for a noun (which
-- is a list of hashes of exteriour dependencies and a jam-encoded noun),
-- load the dependencies from the pin-cushion and reconstruct the value.
loadNoun :: LmdbStore -> Cushion -> DecodeCtx -> ByteString -> IO Fan
loadNoun lmdbStore cache deco vBS = do
    (_refs, !valu) <- decodeBlob loadRefr deco vBS
    pure valu
  where
    loadRefr = loadPinByHash lmdbStore cache []

-- | Write which does something but also commits pins. This makes sure that
-- pins are synced to disk, records their existence in LMDB, and then update
-- `knownPins` to reflect the fact that they have been committed to disk.
withWriteAsyncCommitPins :: LmdbStore
                         -> [PinMetadata_ (Vector Hash256)]
                         -> (MDB_txn -> IO a)
                         -> IO a
withWriteAsyncCommitPins db pins fun = do
  mybTid <- getCurrentTid
  w <- asyncBoundOnCurProcess $
    withCopiedTid mybTid $
      withSimpleTracingEvent "WRITE" "log" $ do
        ensurePinsFsynced db pins

        dbState <- readIORef db.knownPins

        let dbi = db.pinCushion

        (batch, r) <-
          with (writeTxn db.env) $ \txn -> do
            bt <- recordAlreadySyncedPinsInLMDB dbState (logFlags,txn,dbi) pins
            r  <- fun txn
            pure (bt, r)

        -- The lmdb write transaction has committed, mark all pins as committed.
        atomicModifyIORef' db.knownPins \m ->
          let f x (k,v) = insertMap v.hash (PIN_COMMITTED k) x
          in (foldl' f m (mapToList batch), ())

        pure r

  wait w
  where
    logFlags = compileWriteFlags [MDB_NOOVERWRITE, MDB_APPEND]

pinNameToPath :: FilePath -> Hash256 -> FilePath
pinNameToPath dbRoot pinHash = dbRoot </> "pins" </> prefix </> strhash
  where
    strhash = unpack $ Hash256.encodeBtc $ hashToByteString pinHash
    prefix = take 2 strhash

-- -----------------------------------------------------------------------

readTxn :: MDB_env -> Acquire MDB_txn
readTxn env = mkAcquire (mdb_txn_begin env Nothing True) mdb_txn_abort

writeTxn :: MDB_env -> Acquire MDB_txn
writeTxn env = mkAcquireType begin end
  where
    begin =
      withSimpleTracingEvent "BLOCKED" "log" $ do
        mdb_txn_begin env Nothing False

    end txn = \case
        ReleaseException ->
            withSimpleTracingEvent "ABORT" "log" do
                mdb_txn_abort txn
        _ ->
            withSimpleTracingEvent "COMMIT" "log" do
                mdb_txn_commit txn

cursor :: MDB_txn -> MDB_dbi -> Acquire MDB_cursor
cursor txn dbi = mkAcquire (mdb_cursor_open txn dbi) mdb_cursor_close

keyExists :: MdbValue a => MDB_txn -> MDB_dbi -> a -> IO Bool
keyExists txn db key =
    withMdbVal key \mKey ->
        isJust <$> mdb_get txn db mKey

writeKV
    :: (MdbValue a, MdbValue b)
    => MDB_WriteFlags -> MDB_txn -> MDB_dbi
    -> a
    -> b
    -> StoreExn
    -> IO ()
writeKV flags txn db k v exn =
    withSimpleTracingEvent "writeKV" "log" $
    withMdbVal k \mKey ->
    withMdbVal v \mVal -> do
        mdb_put flags txn db mKey mVal >>= \case
            True  -> pure ()
            False -> throwIO exn

-- | Like writeKV, but don't throw an exception.
writeKVNoOverwrite
    :: (MdbValue a,  MdbValue b)
    => MDB_WriteFlags -> MDB_txn -> MDB_dbi
    -> a
    -> b
    -> StoreExn
    -> IO ()
writeKVNoOverwrite flags txn db k v exn = do
    withSimpleTracingEvent "writeKVNoOverwrite" "log" $ do
      (keyExists txn db k) >>= \case
          True  -> pure ()
          False -> writeKV flags txn db k v exn

forAllKV
    :: MDB_cursor
    -> (Ptr MDB_val -> Ptr MDB_val -> IO b)
    -> IO [b]
forAllKV topCur cb = do
      withNullKVPtrs \k v -> do
          mdb_cursor_get MDB_FIRST topCur k v >>= \case
              False -> pure []
              True -> do
                  x <- cb k v
                  continue topCur k v [x]
  where
      continue cur k v xs = do
          mdb_cursor_get MDB_NEXT cur k v >>= \case
              False -> pure $ reverse xs
              True -> do
                  x <- cb k v
                  continue cur k v (x:xs)


foldlRange
    :: MDB_cursor
    -> Word64
    -> b
    -> (b -> Ptr MDB_val -> Ptr MDB_val -> IO b)
    -> IO b
foldlRange topCur startKey initial cb = do
      withMdbValPtr startKey $ \k ->
          withNullPtr $ \v ->
              mdb_cursor_get MDB_SET_KEY topCur k v >>= \case
                  False -> pure initial
                  True -> do
                      x <- cb initial k v
                      continue topCur k v x
  where
      continue cur k v val = do
          mdb_cursor_get MDB_NEXT cur k v >>= \case
              False -> pure $ val
              True -> do
                  x <- cb val k v
                  continue topCur k v x

withNullPtr :: (Ptr MDB_val -> IO a) -> IO a
withNullPtr fn = do
    let nul = MDB_val 0 nullPtr
    allocaBytes (sizeOf nul) $ \pK -> do
        poke pK nul
        fn pK

openDatastore :: FilePath -> Acquire LmdbStore
openDatastore path = mkAcquire (open path) close
