-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire (main, loadFile) where

import Control.Monad.Trans.State.Strict hiding (get, put, modify')
import Control.Monad.State.Class

import PlunderPrelude            hiding (hGetContents)
import Sire.Types
import System.FilePath.Posix

import Data.Vector           ((!))
import Fan                   (Fan(COw, NAT, NAT, REX, ROW, TAb), (%%))
import Fan.Convert           (ToNoun(toNoun), FromNoun(fromNoun))
import Fan.Save              (loadPack, savePack)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr           (castPtr)
import Jelly.Fast.FFI        (c_jet_blake3)
import Loot.Backend          (loadClosure, loadShallow)
import Loot.ReplExe          (closureRex, dieFan, showFan, trkFan)
import Loot.Syntax           (boxRex, keyBox)
import Rex                   (GRex(..), RuneShape(..), TextShape(..), rexLine)
import Rex.Print             (RexColor, RexColorScheme(NoColors))
import Sire.Compile          (compileSire)
import System.Directory      (doesFileExist)
import System.Environment    (getProgName)
import System.IO             (hGetContents)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Char              as C
import qualified Data.List              as L
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Data.Text              as T
import qualified Data.Vector            as V
import qualified Fan                    as F
import qualified Fan.Prof               as Prof
import qualified Rex.Lexer              as Lex
import qualified Rex.Parser             as Rex


-- Local Types -----------------------------------------------------------------

type Any = Fan
type Str = Nat
type Tab = Map
type Rex = GRex Any

newtype Repl a = REPL (StateT Any IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadState Fan)

data Context = CONTEXT
    { file :: !Text
    , line :: !Int
    , rex  :: !Rex
    }
  deriving (Eq, Ord)


type InCtx = (?ctx :: Context)


---------------
-- Functions --
---------------

initialSireStateAny :: Any
initialSireStateAny =
    ROW $ V.fromListN 5 $ [1, 0, TAb mempty, TAb mempty, TAb mempty]

runRepl :: Repl a -> Any -> Any
runRepl (REPL act) ini = unsafePerformIO (execStateT act ini)


-- Loading States --------------------------------------------------------------

-- This lazily loads a state object and crashes if something isn't
-- as expected.  This is intended only for doing queries on specific
-- components of the state, doing a full load in this way is very
-- expensive.

getRow :: Any -> Maybe (Vector Any)
getRow (ROW x) = Just x
getRow _       = Nothing

getLam :: Any -> Any -> Any -> Any -> Any -> Lam
getLam pinnedBit inlinedBit tagNat argsNat bodyVal =
    let
        !pin    = getBit "pinned" pinnedBit
        !inline = getBit "inline" inlinedBit
        !body   = getSyr bodyVal
        !tag    = getNat tagNat    "lambda tag"
        !args   = getNat argsNat   "lambda args"
    in
        LAM{pin,inline,body,args,tag}
  where
    getBit _  (NAT 0) = False
    getBit _  (NAT 1) = True
    getBit cx (NAT n) = error (badBit cx $ show n)
    getBit cx val     = error (badBit cx $ show val)

    badBit cx txt = "bad flag when reading lambda from state: " <> cx <> " " <> txt

getSyr :: Any -> Sire
getSyr (NAT n) = S_VAR n
getSyr topVal  = fromMaybe (error $ "bad Sire AST:\n\n" <> unpack (planText topVal)) do
    params <- getRow topVal
    case toList params of
        [NAT "ref", x]         -> Just $ S_GLO (getBinding x)
        [NAT "val", x]         -> Just $ S_VAL x
        [NAT "app", f, x]      -> Just $ S_APP (getSyr f) (getSyr x)
        [NAT "let", v, b]      -> Just $ S_LET (getSyr v) (getSyr b)
        [NAT "lam", p,i,t,a,b] -> Just $ S_LAM $ getLam p i t a b
        [NAT "lin", x]         -> Just $ S_LIN (getSyr x)
        _                      -> Nothing

getPin :: Any -> Maybe Any
getPin (F.PIN p) = Just p.item
getPin _         = Nothing

getBinding :: Any -> Binding
getBinding bindVal = fromMaybe badBinding $ do
    row <- getPin bindVal >>= getRow

    guard (length row == 5)

    let datum = BINDING_DATA
             { key      = getNat (row!0) "binding key"
             , value    = row ! 1
             , code     = getSyr (row!2)
             , location = row ! 3
             , name     = row ! 4
             }

    pure (BINDING datum bindVal)
  where
    badBinding = error ("Malformed binding:\n" <> unpack (planText bindVal))

getNat :: Any -> String -> Nat
getNat (NAT n) _   = n
getNat _       msg = error msg

getTable :: String -> String -> (Fan -> v) -> Fan -> Map Any v
getTable field ctx getVal = \case
    TAb vals -> map getVal vals
    _        -> error ("invalid `" <> field <> "` field in " <> ctx)


getScope :: Any -> Tab Any Binding
getScope = getTable "scope" "state" getBinding

getPropsTab :: Any -> Tab Any (Tab Any Any)
getPropsTab = getTable "props" "state" (getTable "a-prop" "props" id)

getState :: Any -> SireState
getState stAny = fromMaybe badState $ do
    let (nexVal, ctxVal, scopeVal, modVal, propVal) = getStateFields stAny

    pure $ SIRE_STATE
        { nextKey  = getNat nexVal "invalid `nextKey` field in state"
        , context  = getNat ctxVal "invalid `context` field in state"
        , scope    = getScope scopeVal
        , modules  = getModules modVal
        , allProps = getPropsTab propVal
        }
  where
    badState = error "Malformed sire state"

    getModules :: Any -> Tab Any ( Map Any Binding
                                 , Map Any (Map Any Any)
                                 )
    getModules = getTable "modules" "state"
               $ getPinned "module"
               $ getPair "getState" getScope getPropsTab

getPair :: Text -> (Any -> a) -> (Any -> b) -> (Any -> (a, b))
getPair _ x y (ROW r) | length r == 2 =
    (x (r!0), y (r!1))

getPair ctx _ _ _ = error ("getPair: not a pair (" <> unpack ctx <> ")")

getPinned :: Text -> (Any -> a) -> (Any -> a)
getPinned location getItem = \case
    F.PIN p -> getItem p.item
    _       -> error ("Expected a pin when loading: " <> unpack location)


lookupVal :: Text -> Any -> Maybe Any
lookupVal str stAny = do
    bind <- lookup  (NAT $ utf8Nat str)(getState stAny).scope
    pure bind.d.value


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
    ROW $ V.fromListN 5 [nex, ctxVal, scope, newModules, props]
  where
    ctxVal = NAT (utf8Nat modu)

    (nex, _, _, oldModVal, _) = getStateFields oldSt

    oldModTab = getTable "recover" "modules" id oldModVal

    newModules = TAb (deleteMap ctxVal oldModTab)

    oldScopeVal =
        fromMaybe (error "missing old scope in recovery") $
            lookup ctxVal oldModTab

    (scope, props) = getPair "revert" id id
                   $ fromMaybe (error "module is not a pin (in revert)")
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
    force $ ROW $ V.fromListN 5 [ nextKey
                                , NAT newCtx
                                , TAb mempty
                                , newModules
                                , TAb mempty
                                ]
  where
    (nextKey, oldCtxVal, oldScope, oldModVal, oldProps) = getStateFields oldSt

    oldContext = getNat oldCtxVal "invalid `contenxt` field in state"

    oldModules = case oldModVal of
        TAb t -> t
        _     -> error "`modules` table is not a tab"

    newModules =
        if (oldContext == 0) then
            TAb oldModules
        else
            let ent = F.mkPin $ ROW $ V.fromListN 2 [oldScope, oldProps]
            in TAb (insertMap (NAT oldContext) ent oldModules)


getStateFields :: Any -> (Any, Any, Any, Any, Any)
getStateFields = \case
    ROW v | length v == 5 ->
        (v!0, v!1, v!2, v!3, v!4)
    ROW _ ->
        error "Invalid state object: row does not have five fields"
    _ ->
        error "Invalid state object: not a row"


filterScope :: InCtx => Set Str -> Any -> Any
filterScope whitelist st =
    if not (null bogus)
    then parseFail_ (word "logic error") st
           ("filter for non-existing keys: " <> intercalate ", " bogus)
    else ROW $ V.fromListN 5 [nextKey, context, TAb newScope, modules, binds]
  where
    (nextKey, context, scopeVal, modules, binds) = getStateFields st

    oldScope = case scopeVal of
                   TAb t -> t
                   _     -> error "state.scope is not a TAb"

    filt (NAT k) _ = (k `member` whitelist)
    filt _       _ = error "non-nat key in scope"

    newScope :: Map Any Any
    !newScope = M.filterWithKey filt oldScope

    bogus :: [Text]
    bogus = fmap showKey
          $ filter (not . (`member` oldScope) . NAT) $ toList whitelist

importModule :: InCtx => Rex -> Str -> Maybe (Set Str) -> Any -> Any
importModule blockRex modu mWhitelist stVal =
    ROW $ V.fromListN 5
        $ [nextKey, context, TAb newScope, modulesVal, propsVal]
  where
    moduleBinds :: Tab Any Any
    moduleBinds = either (parseFail_ blockRex stVal) id do
        modules <-
            case modulesVal of
                TAb tab -> Right tab
                _       -> Left "state.modules is not a tab"

        modPairVal <-
            case lookup (NAT modu) modules of
                Just (F.PIN p) -> pure p.item
                Just{}         -> Left "module is not a pin"
                Nothing        -> Left ("Module not loaded: " <> showKey modu)

        case modPairVal of
            ROW r | length r == 2 ->
                case r!0 of
                    TAb t -> Right t
                    _     -> Left "module pinItem is not a tab"
            _ -> Left "module pin is not a pair"

    newBinds :: Map Any Any
    newBinds =
        case mWhitelist of
            Nothing -> moduleBinds
            Just ws ->
                case filter (not . isInModule) (toList ws) of
                    [] -> M.filterWithKey isInWhitelist moduleBinds
                    ss -> parseFail_ blockRex stVal
                              $ (<>) "imported symbols do not exist: "
                                     (tshow $ fmap showKey ss)
              where
                isInModule n = NAT n `member` moduleBinds

                isInWhitelist (NAT n) _ = (n `member` ws)
                isInWhitelist _       _ = False

    newScope :: Map Any Any
    !newScope = case scopeVal of
                   TAb sco -> M.union newBinds sco -- left biased
                   _       -> error "state.scope is not a tab"

    (nextKey, context, scopeVal, modulesVal, propsVal) = getStateFields stVal

{-
    If both maps contain properties for the same binding key, the two
    property-sets are merged.  If two property sets for the same key
    contain the same property, the ones from `x` are chosen.
-}
mergeProps
    :: Map Any (Map Any Any)
    -> Map Any (Map Any Any)
    -> Map Any (Map Any Any)
mergeProps x y = M.unionWith M.union x y

insertBinding
    :: InCtx
    => Rex
    -> (Maybe Nat, Map Any Any, Str, Any, Sire)
    -> Any
    -> Any
insertBinding rx (mKey, extraProps, name, val, code) stVal =
    let
        (nextKeyAny, context, oldScope, modules, oldPropsVal) =
            getStateFields stVal

        !nextKey =
            case nextKeyAny of
                NAT n -> n
                _     -> parseFail_ rx stVal
                             "next-key slot in state is not a nat"

    in case mKey of

        -- If the binding key is not explicitly set, generate a new key
        -- and use that.
        Nothing ->
            insertBinding rx (Just nextKey, extraProps, name, val, code)
                $ ROW
                $ V.fromListN 5
                $ [NAT (nextKey+1), context, oldScope, modules, oldPropsVal]

        Just 0 ->
            parseFail_ rx stVal
                "Trying to create a binding with key=0.  Nonsense!"

        Just key ->
            let
                binding = mkNewBinding $ BINDING_DATA
                    { key      = key
                    , value    = val
                    , code     = code
                    , location = context
                    , name     = NAT name
                    }

                scope = case oldScope of
                            TAb t -> TAb (insertMap (NAT name) binding.noun t)
                            _     -> error "state.scope slot is not a tab"

                oldProps = getPropsTab oldPropsVal

                newProps =
                    if null extraProps then
                        oldProps
                    else
                        mergeProps (M.singleton (NAT key) extraProps) oldProps
            in
                ROW $ V.fromListN 5 [NAT nextKey, context, scope, modules, toNoun newProps]


expand :: InCtx => Any -> Rex -> Repl Rex
expand macro input = do
    st <- get
    case (macro %% st %% REX input %% onErr %% okOk) of
        x@(ROW ro) ->
            case toList ro of
                [NAT msg, REX rex, NAT 0]        -> macroError rex msg
                [REX expansion, newState, NAT 1] -> put newState $> expansion
                [_, _, NAT _]                    -> badExpo x "bad rex"
                [_, _, _]                        -> badExpo x "bad tag"
                _                                -> badExpo x "not arity = 3"
        x                                        -> badExpo x "not row"
  where
    onErr = COw 3 %% NAT 0
    okOk  = COw 3 %% NAT 1

    badExpo x why = parseFail input
                  $ (<>) ("Invalid macro expansion result(" <> why <> ")\n")
                         (planText x)

execute :: InCtx => Rex -> Repl ()
execute rex = do
    stVal <- get
    case rex of
        T{}          -> execExpr rex
        C{}          -> execExpr rex
        N _ rune _ _ -> case (lookupVal rune stVal, rune) of
            ( Just mac, _      ) -> expand mac rex >>= execute
            ( _,        "#="   ) -> doDefine rune rex
            ( _,        "="    ) -> doDefine rune rex
            ( _,        "#*"   ) -> multiCmd rex
            ( _,        "*"    ) -> multiCmd rex
            ( _,        "###"  ) -> doEnter rex
            ( _,        "####" ) -> doEnter rex
            ( _,        "^-^"  ) -> doFilter rune mempty (Just rex)
            ( _,        "#^-^" ) -> doFilter rune mempty (Just rex)
            ( _,        "!!="  ) -> doAssert rune rex
            ( _,        "#!!=" ) -> doAssert rune rex
            ( _,        "<"    ) -> fullPrint rex
            ( _,        "#<"   ) -> fullPrint rex
            ( _,        "#/+"  ) -> doImport rex rune (Just rex)
            ( _,        "/+"   ) -> doImport rex rune (Just rex)
            _ | expRune rune     -> execExpr rex
            _                    -> parseFail rex ("Unbound rune: " <> rune)

{-
    TODO: Caching
-}
main :: RexColor => [FilePath] -> IO ()
main pax = do
  writeIORef F.vShowFan  showFan
  writeIORef F.vTrkFan   trkFan
  writeIORef F.vJetMatch (F.jetMatch)
  handle (\(F.PRIMOP_CRASH op arg) -> dieFan op arg) $
    Prof.withProcessName "Sire" $
    Prof.withThreadName "Sire" do
    let go f = do
            let (dir, fil) = splitFileName f
            case splitExtension fil of
                (modu, ".sire") -> do
                    let modTxt = pack modu
                    ss <- withCache dir \cache -> do
                              doFile dir cache modTxt initialSireStateAny
                    repl (fst ss) (Just modTxt)
                _ -> error "must be given a path to a .sire file"

    case pax of
        []  -> repl initialSireStateAny Nothing
        [f] -> go f
        _   -> usage
  where
    usage :: IO a
    usage = do
        nm <- getProgName
        error ("usage: " <> nm <> " [file]")

-- TODO Take file lock.
withCache
    :: FilePath
    -> (Tab Any Any -> IO (a, Tab Any Any))
    -> IO a
withCache dir act = do
    let fil = takeDirectory dir <.> "cache"
    ex <- doesFileExist fil
    c1 <- if not ex then
              pure mempty
          else do
              byt <- Prof.withSimpleTracingEvent "read" "cache" $ readFile fil
              pak <- Prof.withSimpleTracingEvent "load" "cache" $ loadPack byt
              pure case pak of
                  Left{}        -> mempty
                  Right (TAb t) -> t
                  Right _       -> mempty
    -- trkM $ ROW $ V.fromList ["CACHE", REX $ planRexFull $ TAb c1]
    (x, c2) <- act (c1 :: Tab Any Any)
    p <- F.mkPin' (TAb c2)
    print ("cache hash":: Text, p.hash)
    byt <- Prof.withSimpleTracingEvent "save"  "cache" $ savePack (TAb c2)
    ()  <- Prof.withSimpleTracingEvent "write" "cache" $ writeFile fil byt
    pure x

loadFile :: RexColor => FilePath -> IO Any
loadFile pax = do
    writeIORef F.vShowFan  showFan
    writeIORef F.vTrkFan   trkFan
    writeIORef F.vJetMatch (F.jetMatch)

    let (dir, fil) = splitFileName pax

    case splitExtension fil of
        (modu, ".sire") -> do
            (ss, _hax) <- withCache dir \cache ->
                              doFile dir cache (pack modu) initialSireStateAny
            let scope = (getState ss).scope
            case lookup "main" scope of
                Nothing -> error "No `main` defined in this file"
                Just vl -> pure vl.d.value
        _ ->
            error "must be given a path to a .sire file"

type Lexed = (Int, Text, [(Int, Lex.Frag)])

readRexStream :: FilePath -> Handle -> IO [(Int, Rex)]
readRexStream pax = fmap (blox . lexLns pax . fmap pack . lines) . hGetContents

data PartialBlock = PB
    { depth      :: !Int
    , fstLineNum :: !Int
    , acc        :: ![Lexed]
    }

data BlockState = BS
    { _lineNum   :: !Int
    , _partialSt :: !(Maybe PartialBlock)
    }

initialBlockState :: BlockState
initialBlockState = BS 1 Nothing

blockStep :: BlockState -> Maybe Lexed -> (BlockState, [(Int,[Lexed])])
blockStep = \cases

    -- EOF
    (BS ln Nothing) Nothing   -> (BS ln Nothing, [])
    (BS ln (Just pb)) Nothing -> (BS ln Nothing, wrap pb)

    -- Empty line outside of block
    (BS ln Nothing) (Just (_, t, _)) | blankLine t ->
        (BS (ln+1) Nothing, [])

    -- Empty line during block
    (BS ln (Just pb)) (Just (_, t, _)) | blankLine t ->
        (BS (ln+1) Nothing, wrap pb)

    -- new line during block
    (BS ln (Just pb)) (Just l@(_, _, ts)) ->
        case ts of
           (d,_) : _ | d < pb.depth ->
               (wrap pb <>) <$> blockStep (BS ln Nothing) (Just l)
           _ ->
               (BS (ln+1) (Just pb{acc = l : pb.acc}), [])

    -- comment line outsidie of block
    (BS ln Nothing) (Just (_, _, [])) ->
        (BS (ln+1) Nothing, [])

    -- new line outside of block.  If the first form is closed, it's a
    -- one-line block, otherwise we consume lines until we see a blank
    -- line or EOF.
    (BS ln Nothing) (Just l@(_, _, t:_)) ->
        case t of
            (_, Lex.FORM{}) ->
                (BS (ln+1) Nothing, [(ln, [l])])
            (depth, _) ->
                (BS (ln+1) (Just pb), [])
                  where pb = PB depth ln [l]

  where
    wrap :: PartialBlock -> [(Int, [Lexed])]
    wrap pb = pure (pb.fstLineNum, reverse pb.acc)

    -- lines with comments are not blank (even if no actual rex content)
    blankLine :: Text -> Bool
    blankLine = T.null . T.strip

-- This just converts the `blockStep` state machine into a streaming
-- function and crashes on error.
blox :: [Lexed] -> [(Int, Rex)]
blox = fmap doBlock . go initialBlockState
  where
    go :: BlockState -> [Lexed] -> [(Int, [Lexed])]
    go st []     = snd (blockStep st Nothing)
    go st (l:ls) = let (st', outs) = blockStep st (Just l)
                   in outs <> go st' ls

    doBlock :: (Int, [Lexed]) -> (Int, Rex)
    doBlock (n, ls) =
        case Rex.parseBlock (ls <&> \(_,_,ts) -> ts) of
           Left msg       -> error (unpack msg)
           Right Nothing  -> error "blox: impossible"
           Right (Just r) -> (n, absurd <$> r)

lexLns :: FilePath -> [Text] -> [Lexed]
lexLns fil = go 1
  where
    go :: Int -> [Text] -> [Lexed]
    go _ []     = []
    go n (t:ts) = case Lex.lexLine (fil, n) t of
                      Left msg -> error (unpack msg)
                        -- TODO: throw lexer error
                      Right fs -> (n,t,fs) : go (n+1) ts

inContext :: Text -> Int -> Rex -> (InCtx => IO a) -> IO a
inContext file line rex act =
    let ?ctx = CONTEXT{rex, line, file}
    in try act >>= \case
           Right x -> pure x
           Left (F.PRIMOP_CRASH op arg) ->
               parseFail_ rex ss (planText $ toNoun (op, arg))
                 where ss = initialSireStateAny

runSire :: Text -> Bool -> Any -> [(Int, Rex)] -> IO Any
runSire file inRepl s1 = \case
    []        -> pure s1
    (ln,r):rs -> do
        !es2 <- try $ inContext file ln r
                    $ evaluate
                    $ runRepl (execute  r) (toNoun s1)
        case es2 of
            Right s2 -> runSire file inRepl s2 rs
            Left pc  -> do
                unless inRepl do throwIO (pc :: F.PrimopCrash)
                trkM $ F.ROW $ fromList ["crash", F.NAT pc.errCode, pc.errVal]
                runSire file inRepl s1 rs


doFile :: FilePath -> Tab Any Any -> Text -> Any -> IO ((Any, ByteString), Tab Any Any)
doFile dir c1 modu s1 = do
    let file = modu <> ".sire"
    let pax  = dir </> unpack file

    fileBytes <- readFile pax

    topRexes <- openFile pax ReadMode >>= readRexStream pax

    let moduNoun = NAT (utf8Nat modu)

    case topRexes of

        [] -> do
            let msg  = "Module declarations are required, but this file is empty"
            let rex  = (T TAPE "" Nothing)
            inContext file 0 rex $ parseFail_ rex s1 msg

        -- No <- part means this is the starting point.
        rexes@((_ln, N _ "###" [_] Nothing) : _) -> do
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
                    print (modu, "LOADED FROM CACHE!"::Text)
                    pure ((s3, hax), c1)

                Nothing -> do
                    s2 <- runSire file False s1 rexes
                    let sEnt = switchToContext "REPL" s2
                    let ent  = ROW $ V.fromList [toNoun hax, sEnt]
                    let c2   = insertMap (toNoun modu) ent c1
                    pure ((s2, hax), c2)

        -- There is something before this in the load sequence.
        -- Load that first.
        rexes@((_ln, N _ "###" [_, N _ "<-" [prior] Nothing] Nothing) : _) -> do
            case tryReadModuleName prior of
                Nothing -> terror ("Bad module name: " <> rexText prior)
                Just nm -> do
                  ((s2, predHash), c2) <- doFile dir c1 nm s1
                  Prof.withSimpleTracingEvent (encodeUtf8 modu) "Sire" do

                    -- Massive slow hack, stream two inputs separately.
                    -- (C interface does not currently support this)
                    let bytesToHash = predHash <> fileBytes

                    hax <- allocaBytes 32 \outbuf ->
                           BS.unsafeUseAsCStringLen bytesToHash \(byt, wid) -> do
                               c_jet_blake3 (castPtr outbuf) (fromIntegral wid) (castPtr byt)
                               res <- BS.packCStringLen (outbuf, 32)
                               pure res

                    let mCached = do
                            entry          <- lookup moduNoun c2
                            (cacheKey, st) <- fromNoun entry
                            guard (cacheKey == hax)
                            pure st

                    case mCached of
                        Just s3 -> do
                            let s4 = revertSwitchToRepl modu s3
                            print (modu, "LOADED FROM HASH!"::Text)
                            pure ((s4, hax), c2)

                        Nothing -> do
                            s3 <- runSire file False s2 rexes
                            let sEnt = switchToContext "REPL" s3
                            let ent = ROW $ V.fromList [toNoun hax, sEnt]
                            let c3  = insertMap (toNoun modu) ent c2
                            pure ((s3, hax), c3)

        (ln, rex@(N _ "###" _ _)) : _ ->
            inContext file ln rex
                $ parseFail_ rex s1 "Bad module declaration statement"

        (ln, rex) : _ ->
            inContext file ln rex
                $ parseFail_ rex s1 "All files must start with module declaration"


repl :: Any -> Maybe Text -> IO ()
repl s1 mImport = do

    trkM $ toNoun @Text $ unlines
        [ ""
        , " ==== Sire REPL ===="
        , ""
        , " Since input is multi-line, there is currently no input-prompt."
        , " Just type away!"
        ]

    let s2 = switchToContext "REPL" s1

    -- Pre-load the module listed at the command line.
    s3 <- case mImport of
              Nothing -> pure s2
              Just ng -> do
                  let importRex = N OPEN "/+" [T WORD ng Nothing] Nothing
                  inContext "REPL" 0 importRex do
                      evaluate $ importModule importRex (utf8Nat ng) Nothing s2

    rexes <- readRexStream "REPL" stdin
    _     <- runSire "REPL" True s3 rexes
    pure ()

fullPrint :: InCtx => Rex -> Repl ()
fullPrint rex = do
    case rex of
        N _ _ sons mHeir ->
            case sons <> toList mHeir of
                []  -> pure ()
                [x] -> readExpr [] x                       >>= execDump
                xs  -> readExpr [] (N OPEN "|" xs Nothing) >>= execDump
        _ ->
            error "impossible"


doAssert :: InCtx => Text -> Rex -> Repl ()
doAssert ryn rx = do
    case rx of

        N s r ss (Just heir@(N _ sr _ _)) | ryn==sr -> do
            doAssert ryn (N s r ss Nothing)
            doAssert ryn heir

        rex@(N _ _ sons mHeir) -> do
            trkM (REX rex)
            case sons <> toList mHeir of
                [xRex, yRex] -> do
                    xExp <- readExpr [] xRex
                    yExp <- readExpr [] yRex
                    execAssert (xRex,xExp) (yRex,yExp)
                _ -> do
                    parseFail rex (ryn <> " expects two parameters")
        _ ->
            error "impossible"


doImport :: InCtx => Rex -> Text -> Maybe (Rex) -> Repl ()
doImport blockRex run = \case

    Nothing -> do
        pure ()

    Just (N _ r [moduleRex] h) | run==r -> do
        modu <- readModuleName moduleRex
        modify' (importModule blockRex modu Nothing)
        doImport blockRex run h

    Just (N _ r [moduleRex, (N _ "," symbols Nothing)] h) | run==r -> do
        modu <- readModuleName moduleRex
        syms <- traverse readKey symbols
        modify' (importModule blockRex modu (Just $ setFromList syms))
        doImport blockRex run h

    Just rex -> do
        parseFail rex "Bad import syntax"


doFilter :: InCtx => Text -> Set Nat -> Maybe (Rex) -> Repl ()
doFilter ryn acc = \case

    Nothing ->
        modify' (filterScope acc)

    Just node@(N _ rone sons heir) | ryn==rone -> do
        moreKeys <- setFromList <$> traverse readKey sons
        let overlap = S.intersection acc moreKeys
        unless (null overlap || True) do
            parseFail node ("duplicate symols: " <> tshow overlap)
        doFilter ryn (S.union acc moreKeys) heir

    Just wut -> do
        parseFail wut "Bad export-filter syntax"


multiCmd :: InCtx => Rex -> Repl ()
multiCmd (N _ _ sons mHeir) = traverse_ execute (sons <> toList mHeir)
multiCmd _                  = error "multiCmd: impossible"

doEnter :: InCtx => Rex -> Repl ()
doEnter topRex =
    case topRex of
        N _ _ sons mHeir -> proc (sons <> toList mHeir)
        _                -> error "multiCmd: impossible"
  where
    expect = "Expected something like (### foo) or (### foo <- bar)"

    proc = \case
        [enter, N _ "<-" [from] Nothing] -> do
            target    <- readModuleName enter
            wasJustAt <- readModuleName from
            ss <- getState <$> get
            when (ss.context /= wasJustAt) do
                parseFail topRex "That's not where we were"
            s8 <- get
            let !s9 = switchToContext target s8
            put s9

        [enter] -> do
            target <- readModuleName enter
            ss <- getState <$> get
            unless (ss.context == 0 && null ss.scope) $
                parseFail topRex "### without predecessor, but not in initial state"
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

readExpr :: InCtx => [Maybe Nat] -> Rex -> Repl Sire
readExpr e rex = do
    case rex of
        T{}         -> readPrimExpr e rex
        C{}         -> readPrimExpr e rex
        N _ ryn _ _ -> do
            stVal <- get
            case lookupVal ryn stVal of
                Just macVal -> expand macVal rex >>= readExpr e
                Nothing     -> readPrimExpr e rex

readMultiLine :: InCtx => TextShape -> [Text] -> Maybe Rex -> Repl Sire
readMultiLine ts acc = \case
    Nothing -> pure $ S_VAL $ NAT $ utf8Nat $ unlines $ reverse acc
    Just h  -> case h of
        T s t k | s==ts -> readMultiLine ts (t:acc) k
        _               -> parseFail h "Mis-matched node in text block"

readPrimExpr :: InCtx => [Maybe Nat] -> Rex -> Repl Sire
readPrimExpr e rex = case rex of
    C v              -> pure (S_VAL v)
    T LINE t k       -> readMultiLine LINE [t] k
    T PAGE t k       -> readMultiLine PAGE [t] k
    T _    _ Just{}  -> parseFail rex "leaves cannot have heirs"
    T _    _ Nothing -> readPrimLeaf rex e rex
    N _ r s h        -> readNode r s h

  where
    readNode :: Text -> [Rex] -> Maybe Rex -> Repl Sire
    readNode r s h =
        let ks = s <> toList h
        in case r of
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

    readAnonSig :: Rex -> Repl [Nat]
    readAnonSig (N _ "|" s h) = traverse readKey (s <> toList h)
    readAnonSig n@(T{})       = singleton <$> readKey n
    readAnonSig rx            = parseFail rx "Expected something like: (x y z)"

    readAnonLam :: [Rex] -> Repl Sire
    readAnonLam [sig,bod] = do
        argNames <- readAnonSig sig
        let e2   = reverse (Nothing : fmap Just argNames) <> e
        let args = fromIntegral (length argNames)
        body <- readExpr e2 bod
        pure $ S_LAM $ LAM{tag=0,args,body,pin=False,inline=False}

    readAnonLam [tagRex, sig, bod] = do
        tag <- readKey tagRex
        argNames <- readAnonSig sig
        let e2   = reverse (Nothing : fmap Just argNames) <> e
        let args = fromIntegral (length argNames)
        body <- readExpr e2 bod
        pure $ S_LAM $ LAM{tag,args,body,pin=False,inline=False}

    readAnonLam _ = parseFail rex "Expected two or three parameters"

    readWutSig :: Rex -> Repl (Bool, Nat, [Nat])
    readWutSig topRex@T{} = do
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
        getFuncHead :: Rex -> Repl (Bool, Nat)
        getFuncHead hed@(N _ "**" s h) =
            case s <> toList h of
                [x] -> (True,) <$> readKey x
                _   -> parseFail hed "Expected something like **x"

        getFuncHead hed = (False,) <$> readKey hed

        getBarNode = \case
            N _ "|" s h -> pure (s <> toList h)
            _           -> parseFail topRex "Expecting something like: (f x y)"

    readLam :: Bool -> [Rex] -> Repl Sire
    readLam pin [sigRex, bodRex] = do
        (inline, f, argNames) <- readWutSig sigRex
        let e2   = reverse (Just <$> (f:argNames)) <> e
        let args = fromIntegral (length argNames)
        body <- readExpr e2 bodRex
        pure $ S_LAM $ LAM{tag=f,args,body,pin,inline}

    readLam pin [tagRex, sigRex, bodRex] = do
        tag                   <- readKey tagRex
        (inline, f, argNames) <- readWutSig sigRex
        let e2   = reverse (Just <$> (f:argNames)) <> e
        let args = fromIntegral (length argNames)
        body <- readExpr e2 bodRex
        pure $ S_LAM $ LAM{tag,args,body,pin,inline}

    readLam _ _ = parseFail rex "Expected two or three parameters"

    readRefr :: [Rex] -> Repl Sire
    readRefr [x] = do
        n <- readKey x
        resolveUnqualified rex e n

    readRefr [x,y] = do
        m <- readModuleName x
        n <- readKey y
        resolveQualified rex m n

    readRefr _ = parseFail rex "Needs one or two parameters"

    readKet :: [Rex] -> Repl Sire
    readKet xs = do
        when (length xs < 2) do
            parseFail rex "Needs at least two paramaters"
        v <- readExpr (Nothing : e) (L.last xs)
        b <- traverse (readExpr (Just "_" : e)) (L.init xs)
        pure (S_LET v $ apple_ b)

    readLet :: [Rex] -> Repl Sire
    readLet [nr, vr, br] = do
        n <- readKey nr
        v <- readExpr (Nothing : e) vr
        b <- readExpr (Just n  : e) br
        pure (S_LET v b)
    readLet _ = parseFail rex "Three paramaters are required"

    readLetRec :: [Rex] -> Repl Sire
    readLetRec [nr, vr, br] = do
        n <- readKey nr
        v <- readExpr (Just n : e) vr
        b <- readExpr (Just n : e) br
        pure (S_LET v b)
    readLetRec _ = parseFail rex "Three paramaters are required"

    readLin :: [Rex] -> Repl Sire
    readLin [x] = S_LIN <$> readExpr e x
    readLin _   = parseFail rex "This needs to have only one parameter"

    readApp :: [Rex] -> Repl Sire
    readApp []     = parseFail rex "empty application"
    readApp (r:rx) = do
        (s :| ss) <- traverse (readExpr e) (r :| rx)
        pure (foldl' S_APP s ss)

resolveUnqualified :: InCtx => Rex -> [Maybe Nat] -> Nat -> Repl Sire
resolveUnqualified blockRex e sym = do
    st <- getState <$> get
    case (L.elemIndex (Just sym) e, lookup (NAT sym) st.scope) of
        (Just ng, _) -> pure $ S_VAR (fromIntegral ng)
        (_, Just bn) -> pure $ S_GLO bn
        (_, _)       -> parseFail blockRex ("Unresolved symbol: " <> showKey sym)

resolveQualified :: InCtx => Rex -> Nat -> Nat -> Repl Sire
resolveQualified blockRex modu nam = do
    st <- getState <$> get
    case (lookup (NAT modu) >=> (Just . fst) >=> lookup (NAT nam)) st.modules of
        Just bn -> pure (S_GLO bn)
        Nothing -> parseFail blockRex $ concat [ "Unresolved symbol: "
                                               , showKey modu
                                               , "."
                                               , showKey nam
                                               ]

showKey :: Nat -> Text
showKey = let ?rexColors = NoColors in rexLine . boxRex . keyBox

readPrimLeaf :: InCtx => Rex -> [Maybe Nat] -> Rex -> Repl Sire
readPrimLeaf blockRex e rex =
    case
        asum [ Left . NAT           <$> tryReadNumb rex
             , Right . utf8Nat      <$> tryReadIdnt rex
             , Left . NAT . utf8Nat <$> tryReadCord rex
             ]
             -- TODO: Anything else?
    of
       Nothing        -> parseFail rex "don't know how to parse this leaf"
       Just (Left v)  -> pure (S_VAL v)
       Just (Right n) -> resolveUnqualified blockRex e n


readBindBody :: InCtx => Either Nat ((Bool, Nat), [Nat]) -> Rex -> Repl Sire
readBindBody Left{}                 = readExpr []
readBindBody (Right((_,self),args)) = readExpr $ reverse $ fmap Just $ self:args

execDump :: Sire -> Repl ()
execDump exr =
    let !val = compileSire exr
    in trkM (REX $ planRexFull val)

planRexFull :: Any -> GRex a
planRexFull = fmap absurd . itemizeRexes . closureRex Nothing . loadClosure

execAssert :: InCtx => (Rex, Sire) -> (Rex, Sire) -> Repl ()
execAssert (_xRex, xExp) (_yRex, yExp) = do
    let !xVal = compileSire xExp
    let !yVal = compileSire yExp

    unless (xVal == yVal) do
        let rx = fmap absurd
               $ N OPEN "!!=" []
               $ Just $ N OPEN "*" [planRex xVal]
               $ Just $ N OPEN "*" [planRex yVal]
               $ Nothing
        parseFail rx "ASSERTION FAILURE"

execBind :: InCtx => Rex -> ToBind -> Repl ()
execBind rx (TO_BIND mKey mProp str expr) = do
    let val = compileSire expr
    prp <- getProps rx (compileSire <$> mProp)
    modify' (insertBinding rx (mKey, prp, str, val, expr))
    trkM $ REX $ fmap absurd $ itemizeRexes
                             $ closureRex (Just str) (loadShallow val)

itemizeRexes :: [GRex a] -> GRex a
itemizeRexes [x] = x
itemizeRexes rs  = go rs
  where
    go []     = N OPEN "*" [] Nothing
    go [x]    = N OPEN "*" [x] Nothing
    go (x:xs) = N OPEN "*" [x] (Just $ go xs)

getProps :: InCtx => Rex -> Maybe Any -> Repl (Map Any Any)
getProps _ Nothing          = pure mempty
getProps _ (Just (TAb tab)) = pure tab
getProps r _                = parseFail r "Invalid properties value, must be a tab"

execExpr :: InCtx => Rex -> Repl ()
execExpr rex = do
    expr <- readExpr [] rex
    let val = compileSire expr
    trkM val

doDefine :: InCtx => Text -> Rex -> Repl ()
doDefine ryn rex = do
  case rex of
    N _ _ sons (Just heir@(N _ sub _ _)) | ryn==sub -> do
        readBindCmd rex sons >>= execBind rex
        doDefine ryn heir

    N _ _ sons mHeir -> do
        readBindCmd rex (sons <> toList mHeir) >>= execBind rex

    _ -> error "readDefine: impossible"

readBindCmd :: InCtx => Rex -> [Rex] -> Repl ToBind
readBindCmd rex = \case

    [keyRex, propsRex, binderRex, exprRex] -> do
        key    <- readKey keyRex
        props  <- readExpr [] propsRex
        binder <- readBinder binderRex
        expr   <- readBindBody binder exprRex
        pure $ mkBind (Just key) (Just props) expr binder

    [keyRex, binderRex, exprRex] -> do
        key    <- readKey keyRex
        binder <- readBinder binderRex
        expr   <- readBindBody binder exprRex
        pure $ mkBind (Just key) Nothing expr binder

    [binderRex, exprRex] -> do
        binder <- readBinder binderRex
        expr   <- readBindBody binder exprRex
        pure $ mkBind Nothing Nothing expr binder

    _ -> do
        parseFail rex "Define cmd needs two or three parameters"

  where

    mkBind mKey mProp expr = \case
        Left var ->
            TO_BIND mKey mProp var expr

        Right ((inline, name), argNames) ->
            TO_BIND mKey mProp name
                $ S_LAM
                $ LAM {pin=True, inline, tag=name, args, body=expr}
          where
            args = fromIntegral (length argNames)


word :: Text -> Rex
word t = T WORD t Nothing

open :: Text -> [Rex] -> Rex -> Rex
open r s h = N OPEN r s (Just h)

open_ :: Text -> [Rex] -> Rex
open_ r s  = N OPEN r s Nothing

data ParseFail = PARSE_FAIL
    { block   :: Context
    , problem :: Rex
    , _state  :: Any
    , reason  :: Text
    }
  deriving (Eq, Ord)

parseFailRex :: ParseFail -> Rex
parseFailRex pf =
    id $ open  "#" [word "block",   b.rex]
       $ open  "#" [word "where",   col [word b.file, word (tshow b.line)]]
       $ open  "#" [word "problem", pf.problem]
       $ open_ "#" [word "reason",  word pf.reason]
  where
    b = pf.block
    col ds = N SHUT ":" ds Nothing

data MacroError = MACRO_ERROR
    { block  :: Context
    , input  :: Rex
    , _state :: Any
    , reason :: Text
    }
  deriving (Eq, Ord)

macroErrorRex :: MacroError -> Rex
macroErrorRex me =
    id $ open  "#" [word "block",   me.block.rex]
       $ open  "#" [word "where",   col [word b.file, word (tshow b.line)]]
       $ open  "#" [word "trouble", me.input]
       $ open_ "#" [word "reason",  word me.reason]
  where
    col ds = N SHUT ":" ds Nothing
    b = me.block

macroError :: InCtx => Rex -> Nat -> Repl a
macroError ctx msg = do
    st <- get
    let !me  = MACRO_ERROR ?ctx ctx st (showKey msg)
    let !res = "Macro Failure!" %% REX (macroErrorRex me)
    seq res (error "this should never happen (macroError)")

parseFail :: InCtx => Rex -> Text -> Repl a
parseFail rex msg = do { st <- get; parseFail_ rex st msg }

parseFail_ :: InCtx => Rex -> Any -> Text -> a
parseFail_ rex st msg =
    seq bottom (error "this should never happen (parseFail_)")
  where
    errRex = F.REX $ parseFailRex $ PARSE_FAIL ?ctx rex st msg
    bottom = "Failed to Parse Sire" %% errRex

readBinder :: InCtx => Rex -> Repl (Either Nat ((Bool, Nat), [Nat]))
readBinder rex = do
    case (tryReadKey rex, tryReadLawBinder rex) of
        (Just key, _)  -> pure (Left key)
        (_, Just bind) -> pure (Right bind)
        (_, _)         -> parseFail rex msg
  where
    msg = "Bad binder: expected foo (foo bar), (**foo bar), etc"

tryReadLawBinder :: Rex -> Maybe ((Bool, Nat), [Nat])
tryReadLawBinder rex = do
    kids <- case rex of
                N _ "|" sons heir -> pure (sons <> toList heir)
                _                 -> Nothing
    case kids of
        []                  -> Nothing
        headRex : tailRexes -> do
            (,) <$> tryReadSigHead headRex
                <*> traverse tryReadKey tailRexes

tryReadSigHead :: Rex -> Maybe (Bool, Nat)
tryReadSigHead = \case
    N _ "**" [son] Nothing -> (True,)  <$> tryReadKey son
    rex                    -> (False,) <$> tryReadKey rex


-- Parsing Leaves --------------------------------------------------------------

readKey :: InCtx => Rex -> Repl Nat
readKey rex = maybe bad pure (tryReadKey rex)
  where
    bad = parseFail rex "Bad key: expected something like: 234, foo, 'foo'"

readModuleName :: InCtx => Rex -> Repl Nat
readModuleName rex = maybe bad pure (utf8Nat <$> tryReadModuleName rex)
  where
    bad = parseFail rex "Bad module_name: expected something like foo, 02_foo"


tryReadModuleName :: Rex -> Maybe Text
tryReadModuleName (T _ t Nothing) = Just t
tryReadModuleName _               = Nothing

tryReadKey :: Rex -> Maybe Nat
tryReadKey rex = asum
    [ tryReadNumb rex
    , utf8Nat <$> tryReadIdnt rex
    , utf8Nat <$> tryReadCord rex
    ]

tryReadNumb :: Rex -> Maybe Nat
tryReadNumb = \case
    T WORD t Nothing -> parseNumb t
    _                -> Nothing
  where
    parseNumb :: Text -> Maybe Nat
    parseNumb txt = do
        (c, _) <- T.uncons txt
        guard (c /= '_')
        readMay (filter (/= '_') txt)

tryReadCord :: Rex -> Maybe Text
tryReadCord = \case
    T CORD t Nothing -> Just t
    T TAPE t Nothing -> Just t
    T CURL t Nothing -> Just t
    _                -> Nothing

tryReadIdnt :: Rex -> Maybe Text
tryReadIdnt = \case
    T WORD t Nothing | okIdn t -> Just t
    _                          -> Nothing
  where
    okIdn :: Text -> Bool
    okIdn txt =
        fromMaybe False $ do
            guard (not $ null txt)
            (c, _) <- T.uncons txt
            guard (not $ C.isDigit c)
            pure (all okIdnChar txt)

    okIdnChar '_' = True
    okIdnChar c   = C.isAlphaNum c
