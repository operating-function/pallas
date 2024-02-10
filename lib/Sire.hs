-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire (main, loadFile, planRexFull) where

import Control.Monad.Trans.State.Strict hiding (get, put, modify')
import Control.Monad.State.Class

import Data.Sorted
import PlunderPrelude        hiding (hGetContents)
import Sire.Types
import System.FilePath.Posix

import Data.Text.IO          (hPutStrLn)
import Fan                   (Fan(COw, NAT, NAT, ROW, TAb), (%%))
import Fan.Convert           (FromNoun(fromNoun), ToNoun(toNoun))
import Fan.FFI               (c_jet_blake3)
import Fan.Seed              (LoadErr(..), loadPod, savePod)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr           (castPtr)
import Loot.Backend          (loadClosure, loadShallow)
import Loot.ReplExe          (closureRex, dieFan, showFan, trkFan, trkRex, rexToPex)
import Loot.Syntax           (boxRex, keyBox)
import Rex                   (GRex, rexLine)
import Rex.Print             (RexColor, RexColorScheme(NoColors))
import Sire.Backend          (eval, hasRefTo)
import System.Directory      (doesFileExist)
import System.IO             (hGetContents)
import System.Exit           (exitWith, ExitCode(ExitFailure,ExitSuccess))

import Fan.PlanRex (PlanRex(..), Pex, nounPex, pexNoun)
import Fan.PlanRex (pattern EMBD, pattern EVIL, pattern LEAF, pattern NODE)
import Fan.PlanRex (pattern WORD, pattern TEXT)
import Fan.PlanRex (pattern OPEN, pattern PREF, pattern SHUT, pattern INFX)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Char              as C
import qualified Data.List              as L
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Fan                    as F
import qualified Fan.Prof               as Prof
import qualified Rex                    as Rx
import qualified Rex.Policy             as Rex
import qualified Rex.Mechanism          as Rex


-- Local Types -----------------------------------------------------------------

type Any = Fan
type Str = Nat
type Rex = GRex Any

newtype Repl a = REPL (StateT Any IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState Fan)

data Context = CONTEXT
    { file :: !Text
    , line :: !Int
    , rex  :: !Pex
    }
  deriving (Eq, Ord)


type InCtx = (?ctx :: Context)


---------------
-- Functions --
---------------

mkState :: Any -> Any -> Any -> Any -> Any
mkState nex ctxVal scope modules =
    ROW (arrayFromListN 4 [nex, ctxVal, scope, modules])

initialSireStateAny :: Any
initialSireStateAny = mkState 1 0 (TAb mempty) (TAb mempty)

runRepl :: Repl a -> Any -> Any
runRepl (REPL act) ini = unsafePerformIO (execStateT act ini)


-- Loading States --------------------------------------------------------------

-- This lazily loads a state object and crashes if something isn't
-- as expected.  This is intended only for doing queries on specific
-- components of the state, doing a full load in this way is very
-- expensive.

getRow :: Any -> Maybe (Array Any)
getRow (ROW x) = Just x
getRow _       = Nothing

getLam :: Any -> Any -> Any -> Any -> Any -> Any -> Lam
getLam pinnedBit inlinedBit cycBit tagNat argsNat bodyVal =
    let
        !pin  = getBit pinnedBit  "pinned"
        !mark = getBit inlinedBit "inline"
        !body = getSyr bodyVal
        !recr = getBit cycBit     "is recurisve"
        !tag  = getNat tagNat     "lambda tag"
        !args = getNat argsNat    "lambda args"
    in
        LAM{pin,mark,body,args,tag,recr}
  where
    getBit (NAT 0) _  = False
    getBit (NAT 1) _  = True
    getBit (NAT n) cx = error (badBit cx $ show n)
    getBit val     cx = error (badBit cx $ show val)

    badBit cx txt =
        "bad flag when reading lambda from state: " <> cx <> " " <> txt

getSyr :: Any -> Sire
getSyr (NAT n) = V n
getSyr topVal  = fromMaybe (error $ "bad Sire AST:\n\n" <> unpack (planText topVal)) do
    params <- getRow topVal
    case toList params of
        [NAT "G", x]           -> Just $ G (getBinding "glo" x)
        [NAT "K", x]           -> Just $ K x
        [NAT "A", f, x]        -> Just $ A (getSyr f) (getSyr x)
        [NAT "L", v, b]        -> Just $ L (getSyr v) (getSyr b)
        [NAT "R", v, b]        -> Just $ R (getBinds v) (getSyr b)
        [NAT "F", p,i,c,t,a,b] -> Just $ F $ getLam p i c t a b
        [NAT "M", x]           -> Just $ M (getSyr x)
        _                      -> Nothing
  where
    getBinds :: Fan -> [Sire]
    getBinds (ROW bs) = toList (getSyr <$> bs)
    getBinds _        = error "let binder seq is not a row"

getPin :: Any -> Maybe Any
getPin (F.PIN p) = Just p.item
getPin _         = Nothing

getBinding :: String -> Any -> Bind
getBinding _ctx bindPin = fromMaybe badBinding $ do
    -- case bindPin of
        -- F.PIN _ -> traceM ("getBinding " ++ ctx ++ " (PIN)")
        -- _       -> traceM ("getBinding " ++ ctx ++ " (not a pin!)")
    pin <- getPin bindPin
    -- case pin of
        -- ROW r -> traceM $ "getBinding " <> ctx <> " (ROW): length=" <> show (length r)
        -- _     -> traceM $ "getBinding " <> ctx <> " (not a row!)"
    row <- getRow pin

    guard (length row == 6)

    let datum = BIND_DATA
             { key      = getNat (row!0) "binding key"
             , value    = row!1
             , code     = getSyr (row!2)
             , location = row!3
             , name     = row!4
             , props    = row!5
             }

    pure (BIND datum bindPin)
  where
    bindContents = case bindPin of F.PIN x -> x.item; _ -> "MALFORMED"
    badBinding = error ( "Malformed binding:\n"
                      <> unpack (planText bindPin) <> "\n"
                      <> unpack (planText bindContents)
                       )

getNat :: Any -> String -> Nat
getNat (NAT n) _   = n
getNat _       msg = error msg

getTable :: String -> String -> (Fan -> v) -> Fan -> Tab Any v
getTable field ctx getVal = \case
    TAb vals -> map getVal vals
    _        -> error ("invalid `" <> field <> "` field in " <> ctx)


getScope :: Any -> Tab Any Bind
getScope = getTable "scope" "state" (getBinding "scope")

getState :: Any -> SireState
getState stAny = fromMaybe badState $ do
    let (nexVal, ctxVal, scopeVal, modVal) = getStateFields stAny

    pure $ SIRE_STATE
        { nextKey  = getNat nexVal "invalid `nextKey` field in state"
        , context  = getNat ctxVal "invalid `context` field in state"
        , scope    = getScope scopeVal
        , modules  = getModules modVal
        }
  where
    badState = error "Malformed sire state"

    getModules :: Any -> Tab Any (Tab Any Bind)
    getModules = getTable "modules" "state" (getPinned "module" getScope)

getPinned :: Text -> (Any -> a) -> (Any -> a)
getPinned location getItem = \case
    F.PIN p -> getItem p.item
    _       -> error ("Expected a pin when loading: " <> unpack location)


lookupVal :: Text -> Any -> Maybe Any
lookupVal str stAny = do
    bind <- lookup  (NAT $ utf8Nat str)(getState stAny).scope
    pure bind.bd.value


-- Modifying States ------------------------------------------------------------

-- The cache saves states that have been finalized (and switched into
-- the REPL module).  This saves space because it moves the "current
-- context" into a pin, which will be shared by all ofther modules
-- (instead of having it pinned in later modules, but unpinned in
-- the state)
--
-- However, this is a hack, so we have to undo that hack in order to
-- reconstruct the desired state.
revertSwitchToRepl :: Text -> Any -> Any
revertSwitchToRepl modu oldSt =
    mkState nex ctxVal scope newModules
  where
    ctxVal = NAT (utf8Nat modu)

    (nex, _, _, oldModVal) = getStateFields oldSt

    oldModTab = getTable "recover" "modules" id oldModVal

    newModules = TAb (tabDelete ctxVal oldModTab)

    oldScopeVal =
        fromMaybe (error "missing old scope in recovery") $
            lookup ctxVal oldModTab

    scope = fromMaybe (error "module is not a pin (in revert)")
          $ getPin oldScopeVal

{-
    Because we are constantly threading the sire state through macro,
    we can't keep it around in a type-safe representation.

    Fortunately, the number of state-update operations that are needed
    in Sire itself is quite small.

    This implements all of the state transitions, see `SIRE_SPEC.md`
    for details.
-}

switchToContext :: Str -> Any -> Any
switchToContext newCtx oldSt =
    force (mkState nextKey (NAT newCtx) (TAb mempty) newModules)
  where
    (nextKey, oldCtxVal, oldScope, oldModVal) = getStateFields oldSt

    oldContext = getNat oldCtxVal "invalid `contenxt` field in state"

    oldModules = case oldModVal of
        TAb t -> t
        _     -> error "`modules` table is not a tab"

    newModules =
        if (oldContext == 0) then
            TAb oldModules
        else
            TAb (insertMap (NAT oldContext) (F.mkPin oldScope) oldModules)


getStateFields :: Any -> (Any, Any, Any, Any)
getStateFields = \case
    ROW v | length v == 4 ->
        (v!0, v!1, v!2, v!3)
    ROW _ ->
        error "Invalid state object: row does not have four fields"
    _ ->
        error "Invalid state object: not a row"


filterScope :: InCtx => Set Str -> Any -> Any
filterScope whitelist st =
    if not (null bogus)
    then parseFail_ (WORD "logic error" Nothing) st
           ("filter for non-existing keys: " <> intercalate ", " bogus)
    else mkState nextKey context (TAb newScope) modules
  where
    (nextKey, context, scopeVal, modules) = getStateFields st

    oldScope = case scopeVal of
                   TAb t -> t
                   _     -> error "state.scope is not a TAb"

    filt (NAT k) _ = (k `member` whitelist)
    filt _       _ = error "non-nat key in scope"

    newScope :: Tab Any Any
    !newScope = tabFilterWithKey filt oldScope

    bogus :: [Text]
    bogus = fmap showKey
          $ filter (not . (`member` oldScope) . NAT) $ toList whitelist

importModule :: InCtx => Pex -> Str -> Maybe (Set Str) -> Any -> Any
importModule blockRex modu mWhitelist stVal =
    mkState nextKey context (TAb newScope) modulesVal
  where
    moduleBinds :: Tab Any Any
    moduleBinds = either (parseFail_ blockRex stVal) id do
        modules <-
            case modulesVal of
                TAb tab -> Right tab
                _       -> Left "state.modules is not a tab"

        case lookup (NAT modu) modules of
            Just (F.PIN F.P{item = TAb t}) -> pure t
            Just{}                         -> Left nonPin
            Nothing                        -> Left (notLoaded modu)

    nonPin :: Text
    nonPin = "module is not a pin"

    notLoaded :: Nat -> Text
    notLoaded m = "Module not loaded: " <> showKey m

    newBinds :: Tab Any Any
    newBinds =
        case mWhitelist of
            Nothing -> moduleBinds
            Just ws ->
                case filter (not . isInModule) (toList ws) of
                    [] -> tabFilterWithKey isInWhitelist moduleBinds
                    ss -> parseFail_ blockRex stVal
                              $ (<>) "imported symbols do not exist: "
                                     (tshow $ fmap showKey ss)
              where
                isInModule n = NAT n `member` moduleBinds

                isInWhitelist (NAT n) _ = (n `member` ws)
                isInWhitelist _       _ = False

    newScope :: Tab Any Any
    !newScope = case scopeVal of
                   TAb sco -> tabUnion newBinds sco -- left biased
                   _       -> error "state.scope is not a tab"

    (nextKey, context, scopeVal, modulesVal) = getStateFields stVal

{-
    If both maps contain properties for the same binding key, the two
    property-sets are merged.  If two property sets for the same key
    contain the same property, the ones from `x` are chosen.
mergeProps
    :: Tab Any (Tab Any Any)
    -> Tab Any (Tab Any Any)
    -> Tab Any (Tab Any Any)
mergeProps x y = tabUnionWith tabUnion x y
-}

insertBinding
    :: InCtx
    => Pex
    -> (Nat, Fan, Str, Any, Sire)
    -> Any
    -> Any
insertBinding rx (key, bindProps, name, val, code) stVal =
    let
        (nextKeyAny, context, oldScope, modules) =
            getStateFields stVal
        !nextKey =
            case nextKeyAny of
                NAT n -> n
                _     -> parseFail_ rx stVal
                             "next-key slot in state is not a nat"
    in if key == 0 then
           -- If the binding key is not explicitly set, generate a new key
           -- and use that.
           insertBinding rx (nextKey, bindProps, name, val, code) $
               mkState (NAT (nextKey+1)) context oldScope modules
    else let
        binding = mkNewBind $ BIND_DATA
            { key      = key
            , value    = val
            , code     = code
            , location = context
            , name     = NAT name
            , props    = bindProps
            }
        scope = case oldScope of
                    TAb t -> TAb (insertMap (NAT name) binding.noun t)
                    _     -> error "state.scope slot is not a tab"
    in
        mkState (NAT nextKey) context scope modules

expand :: InCtx => Any -> Pex -> Repl Pex
expand macro input = do
    st <- get
    case (macro %% st %% input.n %% onErr %% okOk) of
        x@(ROW ro) ->
            case toList ro of
                [NAT msg, rex, NAT 0]   -> macroError (nounPex rex) msg
                [expo, newState, NAT 1] -> put newState $> nounPex expo
                [_, _, NAT _]           -> badExpo x "bad rex"
                [_, _, _]               -> badExpo x "bad tag"
                _                       -> badExpo x "not arity = 3"
        x                               -> badExpo x "not row"
  where
    onErr = COw 3 %% NAT 0
    okOk  = COw 3 %% NAT 1

    badExpo x why = parseFail input
                  $ (<>) ("Invalid macro expansion result(" <> why <> ")\n")
                         (planText x)

execute :: InCtx => Pex -> Repl ()
execute rex = do
    stVal <- get
    case rex of
        NODE _ rune _ _ -> case (lookupVal rune stVal, rune) of
            ( Just mac, _      ) -> expand mac rex >>= execute
            ( _,        "#="   ) -> doDefine rune rex
            ( _,        "="    ) -> doDefine rune rex
            ( _,        "#*"   ) -> multiCmd rex
            ( _,        "*"    ) -> multiCmd rex
            ( _,        "####" ) -> doEnter rex
            ( _,        "^-^"  ) -> doFilter rune mempty (Just rex)
            ( _,        "#^-^" ) -> doFilter rune mempty (Just rex)
            ( _,        "=?="  ) -> doAssert rune rex
            ( _,        "#=?=" ) -> doAssert rune rex
            ( _,        "#:|"  ) -> doImport rex rune (Just rex)
            ( _,        ":|"   ) -> doImport rex rune (Just rex)
            _ | expRune rune     -> execExpr rex
            _                    -> parseFail rex ("Unbound rune: " <> rune)

        _ -> execExpr rex

getIndicatedModule :: String -> IO Text
getIndicatedModule pax = do
    let (dir, fil) = splitFileName pax
    unless (dir `elem` okDirs) invalid
    case splitExtensions fil of
        (modu, ".sire") -> pure (pack modu)
        (modu, "")      -> pure (pack modu)
        _               -> invalid
  where

    okDirs :: [String]
    okDirs = [ "", "./", "sire/", "./sire/" ]

    invalid :: a
    invalid = error ("Not a sire module: " <> pax)

{-
    TODO: Caching
-}
main :: RexColor => [String] -> IO ExitCode
main moduleIndicators = do

  modules <- traverse getIndicatedModule moduleIndicators

  writeIORef F.vShowFan  showFan
  writeIORef F.vTrkFan   trkFan
  writeIORef F.vTrkRex   trkRex
  writeIORef F.vJetMatch (F.jetMatch)

  let onCrash (F.PRIMOP_CRASH op arg) = do
          dieFan op arg
          pure (ExitFailure 2)

  handle onCrash $
    Prof.withProcessName "Sire" $
    Prof.withThreadName "Sire" do
    let go preloads modu = do
            (ss, _hax) <- withCache \cache -> do
                              for_ preloads \pre -> do
                                  doFile cache pre initialSireStateAny
                              doFile cache modu initialSireStateAny
            repl ss (Just modu)

    case reverse modules of
        []   -> repl initialSireStateAny Nothing
        m:ms -> go (reverse ms) m

    pure ExitSuccess

-- TODO Take file lock.
withCache :: (IORef (Tab Any Any) -> IO a) -> IO a
withCache act =
    bracket acquire release \(_, vCache) ->
        act vCache
  where
    fil = "./sire.cache"

    acquire :: IO (Tab Any Any, IORef (Tab Any Any))
    acquire = do
        ex <- doesFileExist fil
        c1 <- if not ex then
                  pure mempty
              else do
                  byt <- Prof.withSimpleTracingEvent "read" "cache" $ readFile fil
                  pak <- Prof.withSimpleTracingEvent "load" "cache" $ loadPod byt
                  pure case pak of
                      Left (err :: LoadErr) ->
                          seq (error ("bad cache: " <> show err)) mempty
                      Right pin            ->
                          case pin.item of
                              TAb t -> trace "loaded and hash matches" t
                              _     -> error "bad cache pin"

        vCache <- newIORef c1
        pure (c1, vCache)

    release (c1, vCache) = do
        c2 <- readIORef vCache

        unless (c1 == c2) do
            p <- F.mkPin' (TAb c2)
            hPutStrLn stderr $ tshow ("cache hash":: Text, p.hash)
            eByt <- Prof.withSimpleTracingEvent "save"  "cache" $ try $ savePod p
            case eByt of
               Left (POD_INTEGRITY_CHECK_FAILED hax p2) -> do
                   trkRexM (planRexFull $ toNoun p)
                   trkRexM (planRexFull $ toNoun hax)
                   trkRexM (planRexFull $ toNoun p2)
               Left (e :: LoadErr) -> do
                   trkRexM (planRexFull $ toNoun e)
                   pure ()
               Right byt -> do
                   ()  <- Prof.withSimpleTracingEvent "write" "cache" $
                              writeFile fil byt
                   pure ()

loadFile :: RexColor => FilePath -> IO Any
loadFile moduleIndicator = do
    writeIORef F.vShowFan  showFan
    writeIORef F.vTrkFan   trkFan
    writeIORef F.vTrkRex   trkRex
    writeIORef F.vJetMatch (F.jetMatch)

    modu <- getIndicatedModule moduleIndicator

    (ss, _hax) <- withCache \cache ->
                      doFile cache modu initialSireStateAny
    let scope = (getState ss).scope
    case lookup "main" scope of
        Nothing -> error "No `main` defined in this file"
        Just vl -> pure vl.bd.value

readRexStream :: FilePath -> Handle -> IO [Either Text (Int, Rex)]
readRexStream pax = fmap (blox pax . fmap (encodeUtf8 . pack) . lines) . hGetContents

-- This just converts the `blockStep` state machine into a streaming
-- function and crashes on error.
blox :: FilePath -> [ByteString] -> [Either Text (Int, Rex)]
blox pax = go (Rex.blockState pax)
  where
    foo :: [Rex.Block] -> [Either Text (Int, Rex)]
    foo = (bar <$>)

    bar :: Rex.Block -> Either Text (Int, Rex)
    bar blk =
        case blk.errors of
            e:_ -> Left e
            []  -> Right (blk.lineNum, absurd <$> blk.rex)

    go :: Rex.BlockState -> [ByteString] -> [Either Text (Int, Rex)]
    go st []     = foo $ snd $ Rex.rexStep st Nothing
    go st (b:bs) = let (st2, out) = Rex.rexStep st (Just b)
                   in foo out <> go st2 bs

inContext :: Text -> Int -> Pex -> (InCtx => IO a) -> IO a
inContext file line rex act =
    let ?ctx = CONTEXT{rex, line, file}
    in try act >>= \case
           Right x -> pure x
           Left (F.PRIMOP_CRASH op arg) ->
               parseFail_ rex ss (planText $ toNoun (op, arg))
                 where ss = initialSireStateAny

runSire :: Text -> Bool -> Any -> [Either Text (Int, Pex)] -> IO Any
runSire file inRepl s1 = \case
    []                -> pure s1
    Left msg : rs -> do
        hPutStrLn stderr "\n"
        hPutStrLn stderr msg
        hPutStrLn stderr "\n"
        if inRepl
        then runSire file inRepl s1 rs
        else exitWith (ExitFailure 1)

    Right (ln,r) : rs -> do
        !es2 <- try $ inContext file ln r
                    $ evaluate
                    $ runRepl (execute r) (toNoun s1)
        case es2 of
            Right s2 -> runSire file inRepl s2 rs
            Left pc  -> do
                unless inRepl do throwIO (pc :: F.PrimopCrash)
                trkM $ F.ROW $ arrayFromListN 3
                             $ ["crash", F.NAT pc.errCode, pc.errVal]
                runSire file inRepl s1 rs

doFile :: IORef (Tab Any Any) -> Text -> Any -> IO (Any, ByteString)
doFile vCache modu s1 = do
    let file = modu <> ".sire"
    let pax  = "./sire" </> unpack file

    fileBytes <- readFile pax

    topRexes <- openFile pax ReadMode >>= readRexStream pax

    c1 <- readIORef vCache

    let moduNoun = NAT (utf8Nat modu)

    case fmap (over _2 rexToPex) <$> topRexes of

        [] -> do
            let msg  = "Module declarations are required, but this file is empty"
            let rex  = TEXT "" Nothing
            inContext file 0 rex $ parseFail_ rex s1 msg

        -- No <- part means this is the starting point.
        rexes@(Right (_ln, NODE _ "####" [_] Nothing) : _) -> do
          Prof.withSimpleTracingEvent (encodeUtf8 modu) "Sire" do
            -- Massive slow hack, stream two inputs separately.
            -- (C interface does not currently support this)
            let predHash    = BS.replicate (32::Int) (0::Word8) :: ByteString
            let bytesToHash = predHash <> fileBytes

            hax <- allocaBytes 32 \outbuf ->
                   BS.unsafeUseAsCStringLen bytesToHash \(byt, wid) -> do
                       c_jet_blake3 (castPtr outbuf) (fromIntegral wid) (castPtr byt)
                       res <- BS.packCStringLen (outbuf, 32)
                       pure res

            let mCached = do
                    entry          <- lookup moduNoun c1
                    (cacheKey, st) <- fromNoun entry
                    guard (cacheKey == hax)
                    pure st

            case mCached of
                Just s2 -> do
                    let s3 = revertSwitchToRepl modu s2
                    hPutStrLn stderr $ tshow (modu, "LOADED FROM CACHE!"::Text)
                    pure (s3, hax)

                Nothing -> do
                    s2 <- runSire file False s1 rexes
                    let sEnt = switchToContext "REPL" s2
                    let ent  = ROW $ arrayFromListN 2 [toNoun hax, sEnt]
                    modifyIORef vCache (insertMap (toNoun modu) ent)
                    pure (s2, hax)

        -- There is something before this in the load sequence.
        -- Load that first.
        rexes@(Right (_ln, NODE _ "####" [_, NODE _ "<-" [prior] Nothing] Nothing) : _) -> do
            case tryReadKey prior of
                Nothing -> terror ("Bad module name: " <> pexText prior)
                Just nm -> do
                  (s2, predHash) <- doFile vCache (natUtf8Exn nm) s1
                  Prof.withSimpleTracingEvent (encodeUtf8 modu) "Sire" do

                    -- Massive slow hack, stream two inputs separately.
                    -- (C interface does not currently support this)
                    let bytesToHash = predHash <> fileBytes

                    hax <- allocaBytes 32 \outbuf ->
                           BS.unsafeUseAsCStringLen bytesToHash \(byt, wid) -> do
                               c_jet_blake3 (castPtr outbuf) (fromIntegral wid) (castPtr byt)
                               res <- BS.packCStringLen (outbuf, 32)
                               pure res

                    cacheNow <- readIORef vCache
                    let mCached = do
                            entry          <- lookup moduNoun cacheNow
                            (cacheKey, st) <- fromNoun entry
                            guard (cacheKey == hax)
                            pure st

                    case mCached of
                        Just s3 -> do
                            let s4 = revertSwitchToRepl modu s3
                            hPutStrLn stderr $ tshow (modu, "LOADED FROM CACHE!"::Text)
                            pure (s4, hax)

                        Nothing -> do
                            s3 <- runSire file False s2 rexes
                            let sEnt = switchToContext "REPL" s3
                            let ent = ROW $ arrayFromListN 2 [toNoun hax, sEnt]
                            modifyIORef' vCache $ insertMap (toNoun modu) ent
                            pure (s3, hax)

        Right (ln, rex@(NODE _ "####" _ _)) : _ ->
            inContext file ln rex
                $ parseFail_ rex s1 "Bad module declaration statement"

        Right (ln, rex) : _ ->
            inContext file ln rex
                $ parseFail_ rex s1 "All files must start with module declaration"

        Left msg : _ -> do
            hPutStrLn stderr "\n"
            hPutStrLn stderr msg
            hPutStrLn stderr "\n"
            error "TODO: Include the parsed results as well as the error"
            -- Each error result should also include the processed result!
            -- inContext file ln rex
                -- $ parseFail_ rex s1 "All files must start with module declaration"


repl :: Any -> Maybe Text -> IO ()
repl s1 mImport = do

    trkM $ toNoun @Text $ unlines
        [ ""
        , "==== Sire REPL ===="
        , ""
        , "Since input is multi-line, there is currently no input-prompt."
        , "Just type away!"
        ]

    let s2 = switchToContext "REPL" s1

    -- Pre-load the module listed at the command line.
    s3 <- case mImport of
              Nothing -> pure s2
              Just ng -> do
                  let importRex = OPEN ":|" [WORD ng Nothing] Nothing
                  inContext "REPL" 0 importRex do
                      evaluate $ importModule importRex (utf8Nat ng) Nothing s2

    rexes <- readRexStream "REPL" stdin
    _     <- runSire "REPL" True s3 (fmap (over _2 rexToPex) <$> rexes)
    pure ()

doAssert :: InCtx => Text -> Pex -> Repl ()
doAssert ryn rx = do
    case rx of

        NODE s r ss (Just heir@(NODE _ sr _ _)) | ryn==sr -> do
            doAssert ryn (NODE s r ss Nothing)
            doAssert ryn heir

        rex@(NODE _ _ sons mHeir) -> do
            trkM (pexNoun rex)
            case sons <> toList mHeir of
                [xRex, yRex] -> do
                    xExp <- readExpr [] xRex
                    yExp <- readExpr [] yRex
                    execAssert (xRex,xExp) (yRex,yExp)
                _ -> do
                    parseFail rex (ryn <> " expects two parameters")
        _ ->
            error "impossible"


doImport :: InCtx => Pex -> Text -> Maybe (Pex) -> Repl ()
doImport blockRex run = \case

    Nothing -> do
        pure ()

    Just (NODE _ r [moduleRex] h) | run==r -> do
        modu <- readKey moduleRex
        modify' (importModule blockRex modu Nothing)
        doImport blockRex run h

    Just (NODE _ r [moduleRex, (NODE _ "," symbols Nothing)] h) | run==r -> do
        modu <- readKey moduleRex
        syms <- traverse readKey symbols
        modify' (importModule blockRex modu (Just $ setFromList syms))
        doImport blockRex run h

    Just rex -> do
        parseFail rex "Bad import syntax"


doFilter :: InCtx => Text -> Set Nat -> Maybe (Pex) -> Repl ()
doFilter ryn acc = \case

    Nothing ->
        modify' (filterScope acc)

    Just node@(NODE _ rone sons heir) | ryn==rone -> do
        moreKeys <- setFromList <$> traverse readKey sons
        let overlap = S.intersection acc moreKeys
        unless (null overlap || True) do
            parseFail node ("duplicate symols: " <> tshow overlap)
        doFilter ryn (S.union acc moreKeys) heir

    Just wut -> do
        parseFail wut "Bad export-filter syntax"


multiCmd :: InCtx => Pex -> Repl ()
multiCmd (NODE _ _ sons mHeir) = traverse_ execute (sons <> toList mHeir)
multiCmd _                  = error "multiCmd: impossible"

doEnter :: InCtx => Pex -> Repl ()
doEnter topRex =
    case topRex of
        NODE _ _ sons mHeir -> proc (sons <> toList mHeir)
        _                -> error "multiCmd: impossible"
  where
    expect = "Expected something like (#### foo) or (#### foo <- bar)"

    proc = \case
        [enter, NODE _ "<-" [from] Nothing] -> do
            target    <- readKey enter
            wasJustAt <- readKey from
            ss <- getState <$> get
            when (ss.context /= wasJustAt) do
                parseFail topRex "That's not where we were"
            s8 <- get
            let !s9 = switchToContext target s8
            put s9

        [enter] -> do
            target <- readKey enter
            ss <- getState <$> get
            unless (ss.context == 0 && null ss.scope) $
                parseFail topRex $
                "#### without predecessor, but not in initial state"
            modify' (switchToContext target)

        _ -> do
            parseFail topRex expect

expRune :: Text -> Bool
expRune = (`member` set)
  where
    set :: Set Text
    set = setFromList
        [ "|", "#|", "-", "#-", "**", "#**", "@", "#@", "@@", "#@@"
        , "^", "#^", "&", "#&", "?", "#?", "??", "#??", ".",  "#."
        ]

readExpr :: InCtx => [Maybe Nat] -> Pex -> Repl Sire
readExpr e rex = do
    case rex of
        LEAF{}          -> readPrimExpr e rex
        EMBD{}          -> readPrimExpr e rex
        EVIL{}          -> readPrimExpr e rex
        NODE _ ryn _ _  -> do
            stVal <- get
            case lookupVal ryn stVal of
                Just macVal -> expand macVal rex >>= readExpr e
                Nothing     -> readPrimExpr e rex

readMultiLine :: InCtx => [Text] -> Maybe Pex -> Repl Sire
readMultiLine acc = \case
    Nothing -> pure $ K $ NAT $ utf8Nat $ intercalate "\n" $ reverse acc
    Just h  -> case h of
        LEAF s t k | s==Rx.LINE -> readMultiLine (t:acc) k
        _                       -> parseFail h "Mis-matched node in text block"

readPrimExpr :: InCtx => [Maybe Nat] -> Pex -> Repl Sire
readPrimExpr e rex = case rex of
    EMBD v              -> pure (K v)
    EVIL{}              -> parseFail rex "malformed rex"
    LEAF Rx.LINE t k    -> readMultiLine [t] k
    LEAF _       _ _    -> readPrimLeaf rex e rex
    NODE _ r s h        -> readNode r s h

  where
    readNode :: Text -> [Pex] -> Maybe Pex -> Repl Sire
    readNode r s h =
        let ks = s <> toList h in
        case r of
            "|"   -> readApp       ks
            "#|"  -> readApp       ks
            "-"   -> readApp       ks
            "#-"  -> readApp       ks
            "**"  -> readLin       ks
            "#**" -> readLin       ks
            "@"   -> readLet       ks
            "#@"  -> readLet       ks
            "@@"  -> readLetRec    ks
            "#@@" -> readLetRec    ks
            "^"   -> readKet       ks
            "#^"  -> readKet       ks
            "&"   -> readAnonLam   ks
            "#&"  -> readAnonLam   ks
            "?"   -> readLam False ks
            "#?"  -> readLam False ks
            "??"  -> readLam True  ks
            "#??" -> readLam True  ks
            "."   -> readRefr      ks
            "#."  -> readRefr      ks
            _     -> parseFail rex ("Undefined rune: " <> r)

    readAnonSig :: Pex -> Repl [Nat]
    readAnonSig (NODE _ "|" s h) = traverse readKey (s <> toList h)
    readAnonSig n@(LEAF{})       = singleton <$> readKey n
    readAnonSig rx               = parseFail rx "Expected something like: (x y z)"

    readAnonLam :: [Pex] -> Repl Sire
    readAnonLam [sig,bod] = do
        argNames <- readAnonSig sig
        let e2   = reverse (Nothing : fmap Just argNames) <> e
        let args = fromIntegral (length argNames)
        body <- readExpr e2 bod
        pure $ F $ LAM{tag=0,args,body,pin=False,mark=False,recr=False}

    readAnonLam [tagRex, sig, bod] = do
        tag <- readKey tagRex
        argNames <- readAnonSig sig
        let e2   = reverse (Nothing : fmap Just argNames) <> e
        let args = fromIntegral (length argNames)
        body <- readExpr e2 bod
        pure $ F $ LAM{tag,args,body,pin=False,mark=False,recr=False}

    readAnonLam _ = parseFail rex "Expected two or three parameters"

    readWutSig :: Pex -> Repl (Bool, Nat, [Nat])
    readWutSig topRex@LEAF{} = do
        f <- readKey topRex
        pure (False, f, [])

    readWutSig topRex = do
        kids <- getBarNode topRex
        case kids of
            []     -> parseFail topRex "Expected at least one parameter"
            hed:xs -> do
                (inline, f) <- getFuncHead hed
                args        <- traverse readKey xs
                pure (inline, f, args)
      where
        getFuncHead :: Pex -> Repl (Bool, Nat)
        getFuncHead hed@(NODE _ "**" s h) =
            case s <> toList h of
                [x] -> (True,) <$> readKey x
                _   -> parseFail hed "Expected something like **x"

        getFuncHead hed = (False,) <$> readKey hed

        getBarNode = \case
            NODE _ "|" s h -> pure (s <> toList h)
            _              -> parseFail topRex "Expecting something like: (f x y)"

    readLam :: Bool -> [Pex] -> Repl Sire
    readLam pin [sigRex, bodRex] = do
        (mark, f, argNames) <- readWutSig sigRex
        let e2   = reverse (Just <$> (f:argNames)) <> e
        let args = fromIntegral (length argNames)
        body <- readExpr e2 bodRex
        pure $ F $ LAM{tag=f,args,body,pin,mark,recr=(hasRefTo args body)}

    readLam pin [tagRex, sigRex, bodRex] = do
        tag                 <- readKey tagRex
        (mark, f, argNames) <- readWutSig sigRex
        let e2   = reverse (Just <$> (f:argNames)) <> e
        let args = fromIntegral (length argNames)
        body <- readExpr e2 bodRex
        pure $ F $ LAM{tag,args,body,pin,mark,recr=(hasRefTo args body)}

    readLam _ _ = parseFail rex "Expected two or three parameters"

    readRefr :: [Pex] -> Repl Sire
    readRefr [x] = do
        n <- readKey x
        resolveUnqualified rex e n

    readRefr [x,y] = do
        m <- readKey x
        n <- readKey y
        resolveQualified rex m n

    readRefr _ = parseFail rex "Needs one or two parameters"

    readKet :: [Pex] -> Repl Sire
    readKet xs = do
        when (length xs < 2) do
            parseFail rex "Needs at least two paramaters"
        v <- readExpr e (L.last xs)
        b <- traverse (readExpr (Just "_" : e)) (L.init xs)
        pure (L v $ apple_ b)

    readLet :: [Pex] -> Repl Sire
    readLet [nr, vr, br] = do
        n <- readKey nr
        v <- readExpr e vr
        b <- readExpr (Just n  : e) br
        pure (L v b)
    readLet _ = parseFail rex "Three paramaters are required"

    readLetRec :: [Pex] -> Repl Sire
    readLetRec [vsr, br] = do
        bs <- readBindSeq (Just vsr)
        ks <- pure (fst <$> bs)
        let e' = ((Just <$> ks) <> e)
        vs <- traverse (readExpr e' . snd) bs
        b  <- readExpr e' br
        pure (R vs b)
    readLetRec _ = parseFail rex "Two paramaters are required"

    readBindSeq :: Maybe Pex -> Repl [(Nat, Pex)]
    readBindSeq Nothing = pure []
    readBindSeq (Just (NODE _ "=" [kr,br] h)) = do
        k <- readKey kr
        ((k,br):) <$> readBindSeq h
    readBindSeq (Just _) = do
        parseFail rex "Invalid (=) bind-seq"

    readLin :: [Pex] -> Repl Sire
    readLin [x] = M <$> readExpr e x
    readLin _   = parseFail rex "This needs to have only one parameter"

    readApp :: [Pex] -> Repl Sire
    readApp []     = parseFail rex "empty application"
    readApp (r:rx) = do
        (s :| ss) <- traverse (readExpr e) (r :| rx)
        pure (foldl' A s ss)

resolveUnqualified :: InCtx => Pex -> [Maybe Nat] -> Nat -> Repl Sire
resolveUnqualified blockRex e sym = do
    st <- getState <$> get
    case (L.elemIndex (Just sym) e, lookup (NAT sym) st.scope) of
        (Just ng, _) -> pure $ V (fromIntegral ng)
        (_, Just bn) -> pure $ G bn
        (_, _)       -> parseFail blockRex ("Unresolved symbol: " <> showKey sym)

resolveQualified :: InCtx => Pex -> Nat -> Nat -> Repl Sire
resolveQualified blockRex modu nam = do
    st <- getState <$> get
    case (lookup (NAT modu) >=> Just >=> lookup (NAT nam)) st.modules of
        Just bn -> pure (G bn)
        Nothing -> parseFail blockRex $ concat [ "Unresolved symbol: "
                                               , showKey modu
                                               , "."
                                               , showKey nam
                                               ]

showKey :: Nat -> Text
showKey = let ?rexColors = NoColors in rexLine . boxRex . keyBox

readPrimLeaf :: InCtx => Pex -> [Maybe Nat] -> Pex -> Repl Sire
readPrimLeaf _ e rex@(LEAF s ss (Just heir)) =
   map (lookupVal "#") get >>= \case
       Nothing  -> parseFail rex "leaf-juxtaposition, but no # macro"
       Just hex -> do
           x <- expand hex $ INFX "#" [LEAF s ss Nothing, heir] Nothing
           readExpr e x

readPrimLeaf blockRex e rex =
    case tryReadLeaf rex of
       Just (IDNT n) -> resolveUnqualified blockRex e (utf8Nat n)
       Just (DECI n) -> pure $ K $ NAT n
       Just (CORD n) -> pure $ K $ NAT (utf8Nat n)
       Nothing       -> do
           map (lookupVal "#") get >>= \case
               Just hex -> expand hex (PREF "#" [rex] Nothing) >>= readExpr e
               Nothing  -> parseFail rex "don't know how to parse this leaf"

readBindBody :: InCtx => Either Nat ((Bool, Nat), [Nat]) -> Pex -> Repl Sire
readBindBody Left{}                 = readExpr []
readBindBody (Right((_,self),args)) = readExpr $ reverse $ fmap Just $ self:args

planRexFull :: Any -> GRex a
planRexFull = fmap absurd . itemizeRexes . closureRex Nothing . loadClosure

execAssert :: InCtx => (Pex, Sire) -> (Pex, Sire) -> Repl ()
execAssert (_xRex, xExp) (_yRex, yExp) = do
    let !xVal = eval xExp
    let !yVal = eval yExp

    unless (xVal == yVal) do
        let rx = OPEN "=?=" []
               $ Just $ OPEN "*" [rexToPex $ fmap absurd $ planRex xVal]
               $ Just $ OPEN "*" [rexToPex $ fmap absurd $ planRex yVal]
               $ Nothing
        parseFail rx "ASSERTION FAILURE"

execBind :: InCtx => Pex -> ToBind -> Repl ()
execBind rx (TO_BIND key mProp str expr) = do
    let val = eval expr
    let prp = maybe 0 eval mProp
    modify' (insertBinding rx (key, prp, str, val, expr))
    trkRexM $ fmap absurd
            $ itemizeRexes
            $ closureRex (Just str) (loadShallow val)

itemizeRexes :: [GRex a] -> GRex a
itemizeRexes [x] = x
itemizeRexes rs  = go rs
  where
    go []     = Rx.N Rx.OPEN "*" [] Nothing
    go [x]    = Rx.N Rx.OPEN "*" [x] Nothing
    go (x:xs) = Rx.N Rx.OPEN "*" [x] (Just $ go xs)

execExpr :: InCtx => Pex -> Repl ()
execExpr rex = do
    expr <- readExpr [] rex
    let val = eval expr
    trkM val

doDefine :: InCtx => Text -> Pex -> Repl ()
doDefine ryn rex = do
  case rex of
    NODE _ _ sons (Just heir@(NODE _ sub _ _)) | ryn==sub -> do
        readBindCmd rex sons >>= execBind rex
        doDefine ryn heir

    NODE _ _ sons mHeir -> do
        readBindCmd rex (sons <> toList mHeir) >>= execBind rex

    _ -> error "readDefine: impossible"

readBindCmd :: InCtx => Pex -> [Pex] -> Repl ToBind
readBindCmd rex = \case

    {-
        TODO: Eventually we should be able to kill these hacky "bind
        with props" and "bind with keys" forms.

        Instead of having this as a built-in features in Sire, we should
        define macros that do this, and use those instead.
    -}
    [keyRex, propsRex, binderRex, exprRex] -> do
        key    <- readKey keyRex
        props  <- readExpr [] propsRex
        binder <- readBinder binderRex
        expr   <- readBindBody binder exprRex
        pure $ mkBind key (Just props) expr binder

    [keyRex, binderRex, exprRex] -> do
        key    <- readKey keyRex
        binder <- readBinder binderRex
        expr   <- readBindBody binder exprRex
        pure $ mkBind key Nothing expr binder

    [binderRex, exprRex] -> do
        binder <- readBinder binderRex
        expr   <- readBindBody binder exprRex
        pure $ mkBind 0 Nothing expr binder

    _ -> do
        parseFail rex "Define cmd needs two or three parameters"

  where

    mkBind key mProp body = \case
        Left var ->
            TO_BIND key mProp var body

        Right ((mark, name), argNames) ->
            let recr = hasRefTo args body in
            TO_BIND key mProp name
                $ F $ LAM {pin=True, mark, tag=name, args, body, recr}
          where
            args = fromIntegral (length argNames)

open :: Text -> [Pex] -> Pex -> Pex
open r s h = OPEN r s (Just h)

open_ :: Text -> [Pex] -> Pex
open_ r s  = OPEN r s Nothing

data ParseFail = PARSE_FAIL
    { block   :: Context
    , problem :: Pex
    , _state  :: Any
    , reason  :: Text
    }
  deriving (Eq, Ord)

parseFailRex :: ParseFail -> Pex
parseFailRex pf =
    id $ open  "#" [wrd "block",   b.rex]
       $ open  "#" [wrd "where",   col [wrd b.file, wrd (tshow b.line)]]
       $ open  "#" [wrd "problem", pf.problem]
       $ open_ "#" [wrd "reason",  wrd pf.reason]
  where
    wrd x = WORD x Nothing
    b = pf.block
    col ds = SHUT ":" ds Nothing

data MacroError = MACRO_ERROR
    { block  :: Context
    , input  :: Pex
    , _state :: Any
    , reason :: Text
    }
  deriving (Eq, Ord)

macroErrorRex :: MacroError -> Pex
macroErrorRex me =
    id $ open  "#" [wrd "block",   me.block.rex]
       $ open  "#" [wrd "where",   col [wrd b.file, wrd (tshow b.line)]]
       $ open  "#" [wrd "trouble", me.input]
       $ open_ "#" [wrd "reason",  wrd me.reason]
  where
    wrd x = WORD x Nothing
    col ds = SHUT ":" ds Nothing
    b = me.block

macroError :: InCtx => Pex -> Nat -> Repl a
macroError ctx msg = do
    st <- get
    let !me  = MACRO_ERROR ?ctx ctx st (showKey msg)
    let !res = "Macro Failure!" %% pexNoun (macroErrorRex me)
    seq res (error "this should never happen (macroError)")

parseFail :: InCtx => Pex -> Text -> Repl a
parseFail rex msg = do { st <- get; parseFail_ rex st msg }

parseFail_ :: InCtx => Pex -> Any -> Text -> a
parseFail_ rex st msg =
    seq bottom (error "this should never happen (parseFail_)")
  where
    errRex = pexNoun $ parseFailRex $ PARSE_FAIL ?ctx rex st msg
    bottom = "Failed to Parse Sire" %% errRex

readBinder :: InCtx => Pex -> Repl (Either Nat ((Bool, Nat), [Nat]))
readBinder rex = do
    case (tryReadKey rex, tryReadLawBinder rex) of
        (Just key, _)  -> pure (Left key)
        (_, Just bind) -> pure (Right bind)
        (_, _)         -> parseFail rex msg
  where
    msg = "Bad binder: expected foo (foo bar), (**foo bar), etc"

tryReadLawBinder :: Pex -> Maybe ((Bool, Nat), [Nat])
tryReadLawBinder rex = do
    kids <- case rex of
                NODE _ "|" sons heir -> pure (sons <> toList heir)
                _                 -> Nothing
    case kids of
        []                  -> Nothing
        headRex : tailRexes -> do
            (,) <$> tryReadSigHead headRex
                <*> traverse tryReadKey tailRexes

tryReadSigHead :: Pex -> Maybe (Bool, Nat)
tryReadSigHead = \case
    NODE _ "**" [son] Nothing -> (True,)  <$> tryReadKey son
    rex                    -> (False,) <$> tryReadKey rex


-- Parsing Leaves --------------------------------------------------------------

readKey :: InCtx => Pex -> Repl Nat
readKey rex = maybe bad pure (tryReadKey rex)
  where
    bad = parseFail rex "Bad key: expected something like: 234, foo, 'foo'"

data Leaf = DECI Nat | IDNT Text | CORD Text

-- TODO: Should `tryReadLeaf` also support embeded constant values?

tryReadLeaf :: Pex -> Maybe Leaf
tryReadLeaf = \case
    TEXT t Nothing -> Just (CORD t)
    WORD t Nothing -> tryReadWord t
    _              -> Nothing
  where
    tryReadWord t = do
        (c, _) <- T.uncons t
        if C.isDigit c
        then do guard (all C.isDigit t)
                DECI <$> readMay t
        else Just (IDNT t)

tryReadKey :: Pex -> Maybe Nat
tryReadKey = fmap leafNat . tryReadLeaf

leafNat :: Leaf -> Nat
leafNat = \case { DECI n -> n; IDNT i -> utf8Nat i; CORD s -> utf8Nat s }
