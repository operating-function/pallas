{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Sire.ReplExe
    ( replMain
    , loadFile
    , showFan
    , trkFan
    , dieFan
    , runBlockFan
    , Refr
    , Global(..)
    )
where

import Fan.Print
import Fan.Prof
import Loot.Backend
import PlunderPrelude
import Rex
import Sire.Syntax
import Sire.Types
import Sire.Compile

import Control.Monad.Except       (ExceptT(..), runExceptT)
import Control.Monad.State        (StateT, execStateT, modify')
import Control.Monad.Trans.Except (catchE, throwE)
import Data.Text.IO               (hPutStr, hPutStrLn)
import Loot.ReplExe               (dieFan, printValue, showFan, trkFan)
import Loot.Sugar                 (resugarVal)
import Loot.Syntax                (joinRex, valRex)
import Loot.Types                 (Val(..))
import Rex.Lexer                  (isRuneChar)

import qualified Data.Char      as C
import qualified Data.Map       as M
import qualified Data.Set       as S
import qualified Data.Text      as T
import qualified Fan            as F
import qualified Fan.Prof       as Prof
import qualified Rex.Print.Prim

--------------------------------------------------------------------------------

replMain :: RexColor => [FilePath] -> IO ()
replMain filz = do
    withProcessName "sire" $
      withThreadName "sire" $ do
        handle (\(F.PRIMOP_CRASH o v) -> dieFan o v) do
          vEnv <- newIORef mempty
          vMac <- newIORef mempty
          vPrp <- newIORef (F.TAB mempty)
          vGen <- newIORef (1::Nat)
          vSrc <- newIORef mempty
          writeIORef F.vShowFan  showFan
          writeIORef F.vTrkFan   trkFan
          writeIORef F.vJetMatch (F.jetMatch)
          for_ filz \p ->
              replFile p $ runBlockFan stdout False vSrc vPrp vGen vEnv vMac
          hPutStrLn stdout (Rex.Print.Prim.welcomeComment welcomeTxt)
          replStdin (runBlockFan stdout True vSrc vPrp vGen vEnv vMac)
 where
    welcomeTxt = unlines
        [ ";"
        , "; ==== Sire REPL ===="
        , ";"
        , "; Since input is multi-line, there is currently no input-prompt."
        , "; Just type away!"
        , ";"
        , ""
        ]

loadFile :: RexColor => FilePath -> IO (Maybe Fan)
loadFile file = do
    vEnv <- newIORef mempty
    vMac <- newIORef mempty
    writeIORef F.vShowFan  showFan
    writeIORef F.vTrkFan   trkFan
    writeIORef F.vJetMatch (F.jetMatch)
    vSrc <- newIORef mempty
    vPrp <- newIORef (F.TAB mempty)
    vGen <- newIORef (1::Nat)
    replFile file (runBlockFan stdout False vSrc vPrp vGen vEnv vMac)
    env <- readIORef vEnv
    pure ((.val) <$> lookup cab env)

runBlockFan
    :: RexColor
    => Handle
    -> Bool
    -> IORef (Map Nat Nat)
    -> IORef Fan
    -> IORef Nat
    -> IORef (Map Symb Global)
    -> IORef (Map Text Fan)
    -> Block
    -> IO ()
runBlockFan h okErr vSrc vPrp vGen vEnv vMac block = do
    res <- runExceptT do
        rexed  <- liftEither block.eRex
        env    <- readIORef vEnv
        eVl    <- pure (valFan $ fmap (.val) $ envVal env)
        src    <- readIORef vSrc
        mac    <- readIORef vMac
        parsed <- let ?macEnv = MACRO_ENV src vPrp eVl vGen mac
                  in ExceptT (rexCmd rexed)
        env' <- runCmd h env vSrc vPrp vGen vMac block.input parsed
        writeIORef vEnv env'

    case res of
        Right () -> pure ()
        Left err -> do
            hPutStr stderr
                $ Rex.Print.Prim.errorComment
                $ dent ";;;" err
            unless okErr $ do
                error "EXITING"

cab :: Symb
cab = utf8Nat "_"

vModule :: IORef (Maybe Text)
vModule = unsafePerformIO (newIORef Nothing)

vFiles :: IORef (Map Text (Map Symb Global))
vFiles = unsafePerformIO (newIORef mempty)

data FileState = FILE_STATE
    { declared :: Bool
    , filename :: Text -- module name
    }
  deriving Show

vFileState :: IORef FileState
vFileState = unsafePerformIO $ newIORef $ FILE_STATE False ""

vFileStack :: IORef [Text]
vFileStack = unsafePerformIO (newIORef [])

runFile
    :: RexColor
    => IORef (Map Nat Nat)
    -> IORef Fan
    -> IORef Nat
    -> Text
    -> ExceptT Text IO (Map Symb Global)
runFile vSrc vPrp vGen baseName = do
    oldSt <- readIORef vFileState
    stack  <- readIORef vFileStack

    writeIORef vFileState (FILE_STATE False baseName)
    writeIORef vFileStack (baseName : stack)

    when (elem baseName stack) do
        error $ ("Recurive import: " <>)
              $ unpack
              $ intercalate " -> "
              $ reverse (baseName:stack)

    env <- do
        let fn = unpack ("sire/" <> baseName <> ".sire")
        vEnv <- newIORef mempty
        vMac <- newIORef mempty
        liftIO $ replFile fn
               $ runBlockFan stdout False vSrc vPrp vGen vEnv vMac
        readIORef vEnv

    modifyIORef vFiles (insertMap baseName env)
    writeIORef vFileState oldSt
    writeIORef vFileStack stack

    pure env

openModule
    :: IORef (Map Text Fan)
    -> Map Symb Global
    -> Map Symb Global
    -> IO (Map Symb Global)
openModule vMac moduleVal scope = do
    let pairs = mapToList moduleVal
    execStateT (traverse_ bindVal $ mapToList pairs) scope
  where
    bindVal :: (Symb, Global) -> StateT (Map Symb Global) IO ()
    bindVal (sym, glo) = do
        case natUtf8 sym of
            Right txt | (not (null txt) && all isRuneChar txt) ->
                modifyIORef' vMac (insertMap txt glo.val)
            _ ->
               pure ()
        modify' (insertMap sym glo)

tagify :: Text -> ByteString
tagify =
    encodeUtf8 . take 10 . T.replace " " "-" . T.strip . filter ok
  where
    ok ' ' = True
    ok '-' = True
    ok '_' = True
    ok c   = C.isAlphaNum c

{-
    If we move things into a single pass, which would look something like:

    - OUTPUT is given an rex node instead of a fully parsed Exp
    - OUTPUT calls compileSire on it's parameter.
    - compileSire does further macro-expansion as necessary.

    Then, all of these commands just become "built in macros":

        SireState -> Rex -> (SireState, Rex)
-}
runCmd
    :: RexColor
    => Handle
    -> Map Symb Global
    -> IORef (Map Nat Nat)
    -> IORef Fan
    -> IORef Nat
    -> IORef (Map Text Fan)
    -> Text
    -> XCmd
    -> ExceptT Text IO (Map Symb Global)
runCmd h scope vSrc vPrp vGen vMac itxt cmd = do
  fs <- readIORef vFileState
  unless (fs.filename == "") do
    case cmd of
      MODULE md _ -> do
        unless (fs.declared == False) do
          throwError ("Only one declaration per file: " <> tshow fs)
        unless (fs.filename == md) do
          throwError ( "Module declaration does not match file name: "
                    <> tshow (fs.filename, md)
                     )
        writeIORef vFileState (fs { declared = True } )
      _ -> do
        unless (fs.declared == True) do
          print cmd
          throwError "Module declaration needs to be first thing in a file"

  case cmd of
    FILTER symbs -> do
        for_ symbs \s -> do
            unless (M.member s scope) do
                throwError ("^-^ filtered for undefined: " <> showSymb s)
        let f k _ = S.member k (setFromList symbs)
        pure (M.filterWithKey f scope)

    MODULE md befo -> do
        case befo of
          Nothing -> pure ()
          Just bf -> void (runFile vSrc vPrp vGen bf)

        unless (null scope) do
           throwError "Module must be declared at the beginning of the file"

        old <- readIORef vModule
        case (old, befo) of
            (Just prior, Nothing) -> do
                let m ="### with no prior, but prior is: "
                throwError (m <> prior)
            (Nothing, Just expected) -> do
                let m ="There is no prior module, but ### expected: "
                throwError (m <> expected)
            (Just prior, Just expected) -> do
                when (prior /= expected) do
                    throwError
                      $ unlines
                        [ "Prior does not match what ### expected: "
                        , ""
                        , "    Prior: " <> prior
                        , ""
                        , "    Expected: " <> expected
                        , ""
                        ]
            (Nothing, Nothing) -> do
                pure ()

        writeIORef vModule (Just md)

        pure scope

    IMPORT [] -> do
        pure scope

    IMPORT ((modu, whyt):more) -> do
        let tag = "/+" <> encodeUtf8 modu
        scope' <- profTrace tag "repl" do
            fil <- readIORef vFiles
            tab <- case lookup modu fil of
                     Nothing -> throwError ("Module not loaded: " <> modu)
                     Just tb -> pure tb
            let tab' = case whyt of
                         Nothing -> tab
                         Just sm -> tab & (M.filterWithKey (\k _ -> elem k sm))
            for_ (fromMaybe mempty whyt) \w -> do
                unless (M.member w tab) do
                    throwError $ concat
                        [ "module '"
                        , modu
                        , "' does not export: "
                        , showSymb w
                        ]
            liftIO (openModule vMac tab' scope)
        runCmd h scope' vSrc vPrp vGen vMac itxt (IMPORT more)

    OUTPUT v -> do
        let tag = "<" <> (tagify itxt)
        profTrace tag "repl" do
            glo@(G val _) <- compileSire scope v
            liftIO $ printValue h True Nothing val
            pure $ insertMap cab glo scope

    DUMPIT v -> do
        let tag = "<" <> (tagify itxt)
        profTrace tag "repl" do
            G pln _ <- compileSire scope v
            liftIO $ printValue h False Nothing pln
            pure scope

    ASSERT [] -> pure scope
    ASSERT (TEST_EQL raw v w : more) -> do
        profTrace ("==" <> tagify itxt) "repl" do
            G val _ <- compileSire scope v
            G wal _ <- compileSire scope w
            let NAMED_CLOSURE _ _ vTop = nameClosure (loadShallow val)
            let NAMED_CLOSURE _ _ wTop = nameClosure (loadShallow wal)
            unless (val == wal)
                $ throwError . rexFile
                $ N 0 OPEN "!=" [ (joinRex . valRex . resugarVal mempty) vTop
                                , (joinRex . valRex . resugarVal mempty) wTop
                                ]
                $ Just . joinRex . (\rx -> N 0 OPEN "#!!=" rx Nothing)
                $ fmap (fmap absurd) raw
        runCmd h scope vSrc vPrp vGen vMac itxt (ASSERT more)

    CMDSEQ [] -> pure scope

    CMDSEQ (c:more) -> do
        scope' <- runCmd h scope vSrc vPrp vGen vMac itxt c
        runCmd h scope' vSrc vPrp vGen vMac itxt (CMDSEQ more)

    DEFINE [] -> pure scope

    DEFINE (BIND key nam e : more) -> do
        scope' <- profTrace ("=" <> natBytes nam) "repl" do
            glo@(G pln _) <- compileSire scope e
            liftIO $ printValue h True (Just nam) pln
            case natUtf8 nam of
                Right txt | (not (null txt) && all isRuneChar txt) ->
                    modifyIORef' vMac (insertMap txt pln)
                _ -> pure ()
            pure (insertMap nam glo scope)
        modifyIORef vSrc (insertMap nam key)
        runCmd h scope' vSrc vPrp vGen vMac itxt (DEFINE more)

tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE m = catchE (liftM Right m) (return . Left)

finallyE :: Monad m => ExceptT e m a -> ExceptT e m () -> ExceptT e m a
finallyE m closer = do
    res <- tryE m
    closer
    either throwE return res
{-# INLINE finallyE #-}

profTrace :: MonadIO m => Prof.Name -> Text -> ExceptT e m a -> ExceptT e m a
profTrace tag cat action = do
  Prof.startEvent tag cat
  finallyE action $ do
    Prof.popEvent False mempty

envVal :: Eq a => Map Symb a -> Val a
envVal =
    TAB . mapFromList . fmap f . mapToList
  where
    f :: (Symb, a) -> (Val a, Val a)
    f (nm, v) = (NAT nm, REF v)
