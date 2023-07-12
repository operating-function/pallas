-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Loot.ReplExe
    ( main
    , replMain
    , showFan
    , trkFan
    , dieFan
    , plunRex
    , runBlock
    , printValue
    , renderValue
    , replFile
    , showClosure
    , closureRex
    , showValue
    )
where

import Fan.Print
import Loot
import Loot.Backend
import PlunderPrelude
import Rex
import Jelly.Types

import qualified Rex.Print.Prim

import Control.Exception (throw)
import Data.Text.IO      (hPutStr, hPutStrLn)
import Loot.Sugar        (desugarCmd, resugarRul, resugarVal)

import qualified Data.Text as T
import qualified Fan       as F

--------------------------------------------------------------------------------

main :: IO ()
main = do
    filz <- fmap unpack <$> getArgs
    colorsOnlyInTerminal do
        handle (\(F.PRIMOP_CRASH op arg) -> dieFan op arg) do
            replMain filz

replMain :: RexColor => [String] -> IO ()
replMain filz = do
    writeIORef F.vShowFan  showFan
    writeIORef F.vTrkFan   trkFan
    writeIORef F.vJetMatch F.jetMatch

    vEnv <- newIORef (mempty :: Map Symb Fan)
    for_ filz (\p -> replFile p (runBlock stdout False vEnv))
    welcome stdout
    replStdin (runBlock stdout True vEnv)

welcome :: RexColor => Handle -> IO ()
welcome h = out txt
  where
    out = hPutStr h . Rex.Print.Prim.welcomeComment

    txt = unlines
        [ ";"
        , "; ==== Loot REPL ===="
        , ";"
        , "; Since input is multi-line, there is currently no input-prompt."
        , "; Just type away!"
        , ";"
        , ""
        ]

showError :: RexColor => Handle -> Bool -> Text -> IO ()
showError h tolerateErrors err = do
    liftIO $ hPutStrLn h $ Rex.Print.Prim.errorComment $ dent ";;;" err
    unless tolerateErrors (error "EXITING")

newtype ReplErr = REPL_ERR Text
  deriving newtype (Show)
  deriving anyclass (Exception)

replErr :: Text -> IO a
replErr = throw . REPL_ERR

runBlock
    :: RexColor
    => Handle -> Bool -> IORef (Map Symb Fan) -> Block -> IO ()
runBlock h okErr vEnv (BLK _ _ eRes) = do
    let onErr (REPL_ERR txt) = showError stderr okErr txt
    handle onErr do
        rexed  <- either replErr pure eRes
        env    <- readIORef vEnv
        parsed <- either replErr pure (rexCmd rexed)
        bitter <- pure (let ?env=env in desugarCmd parsed)
        runCmd h vEnv bitter

pinRex :: Symb -> Hash256 -> Val Symb -> Rex
pinRex self _pinKey =
    joinRex . showIt
  where
    hackup (N SHUT_INFIX "-" cs Nothing) = N NEST_PREFIX "|" cs Nothing
    hackup x                             = x

    showIt (LAW ln lt lb) =
        let (t,as,b) = case resugarRul mempty self (RUL ln lt lb) of
                         XLAW t_ as_ b_ -> (t_, as_, b_)
                         XLAM as_ b_    -> (XTAG 0 Nothing, as_, b_)
            vl = hackup (bodRex b)
        in chooseMode vl
             (\vl2 -> absurd<$>(N SHUT_INFIX "=" [xtagApp t as, joinRex vl2] Nothing))
             (\vl2 -> absurd<$>(N OPEN       "=" [xtagApp t as] (Just $ joinRex vl2)))

    showIt v =
        let vl = hackup (valRex (resugarVal mempty v))
        in chooseMode vl
             (\vl2 -> absurd<$>(N SHUT_INFIX "=" [parens [keyRex self], joinRex vl2] Nothing))
             (\vl2 -> absurd<$>(N OPEN       "=" [parens [keyRex self]] (Just $ joinRex vl2)))


plunRex :: Fan -> Rex
plunRex pln = joinRex $ valRex (resugarVal mempty val)
  where
    clz = loadShallow pln
    NAMED_CLOSURE _ _ val = nameClosure clz

-- TODO Can I just rexFile a Loot command?  I remember I was doing this
-- before, but I don't remember why I stopped.
aliasRex :: Maybe Symb -> Val Symb -> GRex Void
aliasRex mSymb vl =
    joinRex rx
  where
    vr = valRex (resugarVal mempty vl)
    rx = case mSymb of
             Nothing   -> chooseMode vr id id
             Just symb -> chooseMode vr
                 (\vr2 -> absurd<$>(N SHUT_INFIX "=" [keyRex symb, joinRex vr2] Nothing))
                 (\vr2 -> absurd<$>(N OPEN "=" [keyRex symb] (Just $ joinRex vr2)))

-- TODO Jank AF.  Much hack.
chooseMode :: GRex a -> (GRex a -> GRex a) -> (GRex a -> GRex a) -> GRex a
chooseMode vr@(N OPEN _ _ _)          _    open = open vr
chooseMode vr@(T THIC_LINE _ _)       _    open = open vr
chooseMode vr@(T THIN_LINE _ _)       _    open = open vr
chooseMode    (N SHUT_INFIX "-" k h)  wide _    = wide (N NEST_PREFIX "|" k h)
chooseMode vr@_                         wide _    = wide vr

printValue
    :: RexColor
    => Handle -> Bool -> Maybe Symb -> Fan -> IO ()
printValue h shallow mBinder vl = do
    let clz = (if shallow then loadShallow else loadClosure) vl
    hPutStrLn h $ showClosure mBinder clz

renderValue :: RexColor => Bool -> Maybe Symb -> Fan -> Text
renderValue shallow mBinder vl =
    let clz = (if shallow then loadShallow else loadClosure) vl
    in showClosure mBinder clz

showClosure :: RexColor => Maybe Symb -> Closure -> Text
showClosure mBinder clz =
    niceLns True $ fmap T.stripEnd $ fmap rexFile $ closureRex mBinder clz

closureRex :: Maybe Symb -> Closure -> [Rex]
closureRex mBinder clz =
    pins <> tops
  where
    NAMED_CLOSURE nam env val = nameClosure clz

    pins = (flip mapMaybe $ toList nam) \n -> do
             lookup n env & \case
                 Nothing      -> Nothing
                 Just (vl, h) -> Just (pinRex n h vl)

    tops = case (pins, mBinder, val) of
             (_, Just n, REF m) | m==n -> []
             (_, Just n, _)            -> [aliasRex (Just n) val]
             (_, Nothing, REF _)       -> []
             ([],Nothing, _)           -> [aliasRex Nothing val]
             (_, Nothing, _)           -> [aliasRex (Just cab) val]


showValue :: Closure -> Rex
showValue clz = aliasRex Nothing val
  where
    NAMED_CLOSURE _ _ val = nameClosure clz

cab :: Symb
cab = utf8Nat "_"

resolveWith
    :: (Show k, Ord k, Traversable f)
    => IORef (Map k Fan) -> f k -> IO (f Fan)
resolveWith vEnv obj = do
    env <- readIORef vEnv
    for obj (\k -> maybe (onErr k) pure (lookup k env))
  where
    onErr = throw . REPL_ERR . ("Unresolved Reference: " <>) . tshow

runCmd
    :: RexColor
    => Handle -> IORef (Map Symb Fan) -> Cmd Fan Symb Symb -> IO ()
runCmd h vEnv =
    go
  where
    resolve :: Traversable f => f Symb -> IO (f Fan)
    resolve = resolveWith vEnv

    go :: Cmd Fan Symb Symb -> IO ()

    go (DEFINE ds) = do
        for_ ds \case
            BIND_PL n r -> do
                rul <- resolve r
                let pln = F.mkPin (ruleFanRaw rul)
                printValue h True (Just n) pln
                modifyIORef' vEnv (insertMap n pln)
            BIND_VL n v -> do
                val <- resolve v
                pln <- pure (valFan val)
                printValue h True (Just n) pln
                modifyIORef' vEnv (insertMap n pln)
            BIND_PN n v -> do
                val <- resolve v
                pln <- pure (F.mkPin $ valFan val)
                printValue h True (Just n) pln
                modifyIORef' vEnv (insertMap n pln)

    go (OUTPUT v) = do
        val <- resolve v
        let pln = valFan val
        printValue h True Nothing pln
        modifyIORef' vEnv (insertMap cab pln)

    go (DUMPIT v) = do
        val <- resolve v
        let pln = valFan val
        printValue h False Nothing pln

    go (ASSERT checks) = do
        for_ checks $ \(raw, v) -> do
            val <- resolve v
            let pln = valFan val
            let NAMED_CLOSURE _ _ top = nameClosure (loadShallow pln)
            unless (top == 1) do
                let heir = joinRex $ valRex raw
                let oneV = T BARE_WORD "1" Nothing
                let expr = joinRex $ valRex $ resugarVal mempty top
                let errE = N OPEN "!=" [oneV, expr] (Just heir)
                throw $ REPL_ERR $ rexFile errE


-- Fan Printer -----------------------------------------------------------------

showFan :: Fan -> Text
showFan =
    let ?rexColors = NoColors
    in showClosure Nothing . loadShallow

trkFan :: RexColor => Fan -> IO ()
trkFan (F.REX r) = putStrLn $ rexFile $ joinRex $ showValue . loadShallow <$> r
trkFan val       = putStrLn $ showClosure Nothing $ loadShallow val

dieFan :: RexColor => Nat -> Fan -> IO ()
dieFan op fan = trkFan $ F.ROW $ fromList ["crash", F.NAT op, fan]
