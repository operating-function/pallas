{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Sire.Syntax
    ( readCmd
    , rexCmd
    , MacroEnv(..)
    )
where

import PlunderPrelude
import Rex
import Sire.Types
import Control.Monad.State

import Fan.Print    (dent)
import Loot.Backend (plunVal, valFan)
import Loot.Types   (LawName(..), Val(..), XTag(..), xtagTag)
import Loot.Syntax  (readArgs, readSigy)

import qualified Fan
import qualified Loot.ReplExe as Loot
import qualified Loot.Sugar   as Loot
import qualified Loot.Syntax  as Loot


-- Types -----------------------------------------------------------------------

{-
    TODO: (.env) should be (Map Nat Fan)

        If it isn't a tab, coerce it.

    TODO: (.sources) should map from bindings to their identities:

        (.sources) should map from identifiers to the source-node that
        was used to define it (the identity of the rex-node leaf of the
        name of the binding):

            %% =mkLaw  3
            %% =rexVal 23
            %% =map    2352

        This should be updated every time we update the env table.

    TODO: Eliminate (.macros) and just use (.env)

        If it isn't a tab, coerce it.

        Current thing is *WRONG* even, since having macros that explicitly
        add a rune-named binding to the globals object really should
        have that work as a macro.

    TODO: Add (attrs :: Map Nat (Map Nat Fan))

        Make sure that the conversion when moving in between Haskell
        and Sire is lazy, don't rebuild the whole structure every time.

-}

data MacroEnv = MACRO_ENV
    { sources  :: Map Nat Nat
    , props    :: IORef Fan
    , env      :: Fan
    , attrNext :: IORef Nat
    , macros   :: Map Text Fan
    }

--
-- Also, how can we have inlining work, if every macro-expansion is
-- allowed to freely alter these values?
--
-- I suppose we can use caching?  Inlining uses the %src and %env
-- properties.
--
-- That is "compiled into an internal AST", and that value is cached?
-- Wont those lookups be expensive?  Comparing whole environments, etc.
--
-- Yes, the Ord instance is pretty slow.  The `Eq` instance at least
-- short-circuits on size.
--
-- Hashing also seems expensive.
--
-- We could define a newtype wrapper that has better instances.
--
-- (Source, Environment) -> ResolvedAST
--
-- Alternately, we could change the interface so that each expansion
-- produces a *diff* instead of a new value.
--
-- The returned environment is all the new bindings (or zero to remove
-- a binding).
--
-- The returned properties table is merged in (no deletions are allowed).
--
-- The returned `nex` value is the number of allocations made?  Or still
-- a state?
--

type HasMacroEnv = (?macEnv :: MacroEnv)

type Red = ReadT Fan IO


-- Parsing ---------------------------------------------------------------------

impartIdentity :: GRex a -> State Nat (GRex a)
impartIdentity = go
  where
    key :: State Nat Nat
    key = do
        !nex <- get
        traceM (show nex)
        put $! (nex+1)
        pure nex

    tgo :: Traversable t => t (GRex a) -> State Nat (t (GRex a))
    tgo x = traverse go x

    go = \case
        C c          -> pure (C c)
        T _ ty t h   -> T <$> key <*> pure ty <*> pure t <*> tgo h
        N _ ty r s h -> N <$> key <*> pure ty <*> pure r <*> tgo s <*> tgo h

rexCmd :: (RexColor, HasMacroEnv) => Rex -> IO (Either Text XCmd)
rexCmd rex = do
    nex <- readIORef ((?macEnv).attrNext)
    let (rex', nex') = runState (preProcess rex) nex
    writeIORef ((?macEnv).attrNext) nex'
    pure $ Loot.resultEitherText dropEmbed rex
         $ runReading readCmd
         $ fmap absurd
         $ rex'
  where
    preProcess = impartIdentity . rewriteMultiLineString

    -- This is a dumb hack.  How to do for real?
    dropEmbed :: GRex Fan -> Rex
    dropEmbed = \case
        T k s t h     -> T k s t                    (dropEmbed <$> h)
        C c           -> T 0 THIN_CORD (tshow c)    Nothing
        N k m r xs mK -> N k m r (dropEmbed <$> xs) (dropEmbed <$> mK)

-- This combines sequences of line-strings into a single line-string
-- that contains newlines.
--
-- This is a big hack to work around the fact that ReadT doesn't currently
-- offer any way to match against this atom+heir pattern.
--
-- TODO Resolve this, this is a very stupid hack!
--
rewriteMultiLineString :: Rex -> Rex
rewriteMultiLineString = go
  where
    go = \case
      T k s@THIC_LINE t (Just x) -> multiLine k s [t] (Just x)
      T k s@THIN_LINE t (Just x) -> multiLine k s [t] (Just x)
      T k s t h                  -> T k s t (go <$> h)
      N k m r p h                -> N k m r (go <$> p) (go <$> h)
      C x                        -> absurd x

    multiLine k style acc mHeir =
        case mHeir of
            Just (T _ style2 txt heir2) | style==style2 ->
                multiLine k style (txt:acc) heir2
            _ ->
                T k style (intercalate "\n" $ reverse acc) (go <$> mHeir)

runReading :: Red a -> GRex Fan -> Result Fan a
runReading act = unsafePerformIO . runResultT . runReadT act

readModuleName :: Red Text
readModuleName = matchLeaf "module_name" (Just . snd)

readImports :: Red [(Text, Maybe (Set Symb))]
readImports = do
    rune "/+"
    asum
        [ do i <- form1 readModuleName
             pure [(i, Nothing)]
        , do (i,xs) <- form1C readModuleName readImports
             pure ((i,Nothing):xs)
        , do (i,rs) <- form2 readModuleName nameList
             pure [(i, Just (setFromList rs))]
        , do (i,rs,xs) <- form2C readModuleName nameList readImports
             pure ((i, Just (setFromList rs)):xs)
        ]
  where
    nameList = (rune "," >> formN Loot.readKey)

readFilter :: Red [Symb]
readFilter = do
    (rune "#^-^" <|> rune "^-^")
    formNe Loot.readKey readFilter <&> \case
        (is, Nothing)   -> is
        (is, Just more) -> is <> more

readCmd :: (RexColor, HasMacroEnv) => Red XCmd
readCmd = do
    let doMacro (r,h) = (rune r >> runMacro readCmd h)
    let macros = doMacro <$> mapToList ?macEnv.macros
    asum (macros <> fixed <> [OUTPUT <$> readExpr])
  where
    fixed =
        [ (rune "#=" <|> rune "=") >> readDefine
        , IMPORT <$> readImports
        , FILTER <$> readFilter
        , do (rune "#!!=" <|> rune "!!=")
             ((pr,p), (qr,q), mC) <-
                 let x = withRex readExpr
                 in (form2C x x readCmd <&> \(a,b,c) -> (a,b,Just c))
                <|> (form2 x x          <&> \(a,b)   -> (a,b,Nothing))
             pure (ASSERT [pr,qr] p q mC)
        , do (rune "#<" <|> rune "<")
             (v,vs) <- form1Nc readExpr readExpr
             pure (DUMPIT $ foldl' EAPP v vs)
        ]

    withRex :: Red v -> Red (Rex, v)
    withRex red = (,) <$> getRex <*> red
      where
       getRex = fmap (const $ error "impossible") <$> readRex

    readDefine = do
        ((t,args), b, andThen) <- readTopBinder
        let nm = xtagIdn t
            ky = xtagKey t
            tg = xtagTag t
            c  = Nothing
            res = case args of
                    []   -> BIND_EXP ky nm c b
                    r:rs -> BIND_FUN ky nm c (FUN nm (LN tg) (r:|rs) b)
        pure $ DEFINE res andThen

readTopBinder
    :: (RexColor, HasMacroEnv)
    => Red ( (XTag, [Symb])
           , Exp Fan Symb Symb
           , Maybe (Cmd Fan Symb Symb)
           )
readTopBinder =
    asum
    [ form2C b x c >>= \(bv, xv, cv) -> pure (bv ,xv, Just cv)
    , form1C b x   >>= \(bv, xv)     -> pure (bv, xv, Nothing)
    , form2  b x   >>= \(bv, xv)     -> pure (bv, xv, Nothing)
    ]
 where
  (b, x, c) = (readBinder, readExpr, readCmd)

  readBinder :: Red (XTag, [Symb])
  readBinder = simple <|> complex
    where
      simple = (,[]) . Loot.simpleTag <$> withIdent Loot.readBymb
      complex = rune "|" >> form1N Loot.readXTag Loot.readSymb



-- Functions -------------------------------------------------------------------

valFanRex :: RexColor => Val Fan -> GRex v
valFanRex = fmap absurd
          . Loot.joinRex
          . Loot.valRex
          . Loot.resugarVal mempty
          . fmap (utf8Nat . rexLine . Loot.plunRex)

readSymb :: Red Symb
readSymb = Loot.readIdnt
       <|> (rune "##" >> form1 Loot.readKey)

readText :: Red Text
readText = matchLeaf "page" \case
    (THIC_LINE, l) -> Just l
    (THIN_LINE, l) -> Just l
    (THIN_CORD, t) -> Just t
    (THIC_CORD, t) -> Just t
    (CURL_CORD, t) -> Just t
    (BARE_WORD, _) -> Nothing

readNumb :: Red Nat
readNumb =
    matchName "nat" \nam ->
        case headMay nam of
            Just '_' -> Nothing
            _        -> readMay (filter (/= '_') nam)

readExpr :: (RexColor, HasMacroEnv) => Red XExp
readExpr = do
    let doMacro (r,h) = (rune r >> runMacro readExpr h)
    let macros = doMacro <$> mapToList ?macEnv.macros
    asum (macros <> fixed)
  where
    fixed =
        [ EBED <$> readExtra
        , ENAT <$> readNumb
        , ENAT <$> (utf8Nat <$> readText)
        , EREF <$> readSymb

        , do (rune "#^" <|> rune "^")
             (xs, h) <- formN1c readExpr readAppHead
             pure case h of
                 Left f  -> ELIN (EREF f :| xs)
                 Right f -> foldl' EAPP f xs

        , do (rune "#@@" <|> rune "@@")
             (r,v,b) <- form3c readSymb readExpr readExpr
             pure (EREC r v b)

        , do (rune "#??" <|> rune "??")
             ((t,rs),b) <- form2c readSigy readExpr
             pure (ELAM True (FUN (xtagIdn t) (LN $ xtagTag t) rs b))

        , do (rune "#?" <|> rune "?")
             ((t,rs),b) <- form2c readSigy readExpr
             pure (ELAM False (FUN (xtagIdn t) (LN $ xtagTag t) rs b))

        , do (rune "#&" <|> rune "&")
             (rs, b) <- form2c readArgs readExpr
             pure (ELAM False (FUN 0 (LN 0) rs b))

        , do (rune "#|" <|> rune "|" <|> rune "-")
             (h, xs) <- form1Nc readAppHead readExpr
             pure case h of
                 Left f  -> ELIN (EREF f :| xs)
                 Right f -> foldl' EAPP f xs

        , do (rune "#@" <|> rune "@")
             (n, v, b) <- form3c readSymb readExpr readExpr
             pure (ELET n v b)
        ]

readAppHead :: (RexColor, HasMacroEnv) => Red (Either Symb (Exp Fan Symb Symb))
readAppHead =
  (Left <$> (rune "*" >> form1 readSymb)) <|> (Right <$> readExpr)


-- Macro Runner ----------------------------------------------------------------

sourcesTable :: Map Nat Nat -> Fan
sourcesTable = Fan.TAB . mapFromList . fmap f . mapToList
  where
    f :: (Nat, Nat) -> (Fan, Fan)
    f (k, v) = (Fan.NAT k, Fan.NAT v)

-- TODO Better to just use a parameter instead of IO monad?
-- TODO This can be simplified even more.
runMacro :: (RexColor, HasMacroEnv) => Red a -> Fan -> Red a
runMacro reader macroVal = do
    aft <- readIORef ?macEnv.attrNext
    prp <- readIORef ?macEnv.props
    nod <- readRex
    --
    let src = sourcesTable (?macEnv).sources
    --
    let newVal = plunVal . valFan
               $ REF macroVal
                   `APP` REF src              {-  s  -}
                   `APP` REF prp              {-  p  -}
                   `APP` REF ?macEnv.env      {-  e  -}
                   `APP` NAT aft              {-  a  -}
                   `APP` REF (Fan.REX nod)    {-  r  -}
                   `APP` COW 2                {- err -}
                   `APP` COW 3                {- ok  -}
    --
    case loadMacroExpansion newVal of
        Left (rex, msg) ->
            let ?rexColors = NoColors in
            throwError $ (msg <>)
                       $ ("\n\nIn Sub-Expression:\n\n" <>)
                       $  dent "   "
                       $ rexFile
                       $ Loot.joinRex
                       $ fmap Loot.plunRex rex
        Right (!p, !a, !r) -> do
            writeIORef ?macEnv.props p
            writeIORef ?macEnv.attrNext a
            liftIO (runResultT (runReadT reader r)) >>= readResult


-- Macro Loader ----------------------------------------------------------------

loadMacroExpansion
    :: RexColor
    => Val Fan
    -> Either (GRex Fan, Text) (Fan, Nat, GRex Fan)
loadMacroExpansion = \case

    ROW (toList -> [r,a,p]) -> do
        expr <- getRex r
        prop <- case valFan p of
                 vl@Fan.TAB{} -> pure vl
                 _            -> Left (valFanRex p, "BAD PROPERTIES")
        atNx <- case valFan a of
                  Fan.NAT n -> pure n
                  _         -> Left (valFanRex a, "BAD NEXT_ATTRIBUTE_KEY")
        Right (prop, atNx, expr)

    ROW (toList -> [msg, ctx]) -> do
        expr <- getRex ctx
        mesg <- case msg of
                  NAT n -> pure $ decodeUtf8 $ natBytes n
                  _     -> Left (valFanRex msg , "BAD ERROR_MSG")
        Left (expr, mesg)

    fan -> do
        Left (valFanRex fan, errMsg)

  where

    errMsg = ( "Invalid macro expansion.  "
            <> "Expected a value like [ctx msg] or [p a r]"
             )

    getRex :: Val Fan -> Either (GRex fan, Text) (GRex Fan)
    getRex (REX x) = Right (valFan <$> x)
    getRex notRx   = Left (valFanRex notRx, "Is not rex")
