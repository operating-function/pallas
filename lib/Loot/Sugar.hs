-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Loot.Sugar
    ( desugarCmd
    , resugarRul
    , resugarVal

    , desugarBod
    , desugarLaw
    , desugarRul
    , desugarVal

    , varNames
    , HasEnv
    )
where

import Loot.Syntax
import Loot.Types
import PlunderPrelude

import Data.List    ((!!))
import Fan          (Fan, (%%))
import Loot.Backend (isValCodeShaped)


-- Types -----------------------------------------------------------------------

type HasEnv = (?env :: Map Symb Fan)


-- Desugaring ------------------------------------------------------------------

desugarCmd :: HasEnv => XCmd z -> Cmd z Symb Symb
desugarCmd (XOUTPUT v)  = OUTPUT (desugarVal v)
desugarCmd (XDUMPIT v)  = DUMPIT (desugarVal v)
desugarCmd (XASSERT cs) = ASSERT (cs <&> \c -> (c, desugarVal c))
desugarCmd (XDEFINE ds) = DEFINE (go <$> ds)
  where
    go (XBIND_VL n v) = BIND_VL n (desugarVal v)
    go (XBIND_PN n v) = BIND_PN n (desugarVal v)
    go (XBIND_PL r)   = BIND_PL t (desugarRul r)
                        where
                          t = tagIdn $ desugarTag $ xlName r

desugarTag :: XTag -> Tag
desugarTag = \case
    XTAG idn (Just nam) -> TAG idn (LN nam)
    XTAG idn Nothing    -> TAG idn (LN idn)

resugarTag :: Tag -> XTag
resugarTag (TAG nm (LN tg)) | nm==tg = XTAG nm Nothing
resugarTag (TAG nm (LN tg))          = XTAG nm (Just tg)

desugarVal :: HasEnv => XVal -> Val Symb
desugarVal = \case
    XVREF g   -> REF g
    XVNAT n   -> NAT n
    XVAPP f x -> APP (desugarVal f) (desugarVal x)
    XVROW r   -> ROW (desugarVal <$> r)
    XVTAB t   -> TAB (goPair <$> t)
    XVBAR b   -> BAR b
    XVLAW l   -> desugarLaw l
    XVCOW n   -> COW n
    XVSET n   -> SET (desugarVal <$> n)
    XVROX x   -> ROX (desugarVal <$> x)
  where
    goPair (k,v) = (desugarVal k, desugarVal v)

desugarRul :: HasEnv => XLaw -> Rul Symb
desugarRul (XLAW t as b) =
    let ar = fromIntegral (length as)
        tg = desugarTag t
        nex = succ $ fromIntegral $ length as
        loc = mapFromList $ zip (tagIdn tg : toList as) [0..]
    in RUL (tagNam tg) ar (desugarBod nex loc b)
desugarRul (XLAM as b) =
    let ar = fromIntegral (length as)
        nex = succ $ fromIntegral $ length as
        loc = mapFromList $ zip (toList as) [1..]
    in RUL (LN 0) ar (desugarBod nex loc b)

desugarLaw :: HasEnv => XLaw -> Val Symb
desugarLaw l = LAW n a b
  where
    RUL n a b = desugarRul l

desugarBod :: HasEnv => Int -> Map Symb Int -> XBod -> Bod Symb
desugarBod = go
  where
    go :: Int -> Map Symb Int -> XBod -> Bod Symb
    go n e = \case
        XRAW v     -> raw n (desugarVal v)
        XVAR v     -> maybe (raw n $ REF v) (BVAR . fromIntegral) (lookup v e)
        XCNS v     -> BCNS (desugarVal v)
        XAPP f x   -> BAPP (go n e f) (go n e x)
        XBAD v     -> BBAD (desugarVal v)
        XLET v x b -> BLET (go n' e' x) (go n' e' b)
                       where n' = n+1
                             e' = insertMap v n e

    resolve :: Symb -> Fan
    resolve k = fromMaybe (0 %% 0 %% 0) (lookup k ?env)

    raw :: Int -> Val Symb -> Bod Symb
    raw nex v =
        if isValCodeShaped maxArg (resolve <$> v)
        then BCNS v
        else BBAD v
      where
        maxArg = fromIntegral (nex-1)


-- Resugaring ------------------------------------------------------------------

resugarVal :: Set Symb -> Val Symb -> XVal
resugarVal pu = \case
    REF x     -> XVREF x
    NAT n     -> XVNAT n
    APP f x   -> XVAPP (go f) (go x)
    ROW r     -> XVROW (go <$> r)
    LAW n a b -> XVLAW (resugarLaw pu n a b)
    TAB t     -> XVTAB (goPair <$> t)
    BAR b     -> XVBAR b
    COW n     -> XVCOW n
    SET n     -> XVSET (go <$> n)
    ROX n     -> XVROX (go <$> n)
  where
    go = resugarVal pu
    goPair (k,v) = (go k, go v)

-- TODO Need a symbol table in order to choose valid names.
-- TODO Now we have one! (pu)!  Use it!
resugarLaw :: Set Symb -> LawName -> Nat -> Bod Symb -> XLaw
resugarLaw pu ln@(LN n) a b = resugarRul pu n (RUL ln a b)

recursiveBod :: Bod a -> Bool
recursiveBod = \case
    BVAR 0   -> True
    BVAR _   -> False
    BCNS{}   -> False
    BBAD{}   -> False
    BAPP f x -> recursiveBod f || recursiveBod x
    BLET v x -> recursiveBod v || recursiveBod x

resugarRul :: Set Symb -> Symb -> Rul Symb -> XLaw
resugarRul pu idn rul@(RUL nam arg bod) =
    if anon
    then XLAM argNames body
    else XLAW yNam argNames body
  where
    body = goBod weUsed isFree supply nex bod
    anon = (LN 0 == nam) && not (recursiveBod bod)
    isFree  = (`elem` freeSet)
    freeSet = setFromList (toList bod) :: Set Symb
    yNam = resugarTag $ TAG idn nam
    nex = 1 + arg
    argNames = case take (fromIntegral arg) argSupply of
                   []   -> error "resugarRul: impossible"
                   x:xs -> (x:|xs)
    supply = idn : argSupply
    weUsed = union pu $ setFromList $ (idn:) $ take (numBindsUsed rul) varNames
    argSupply = filter (not . isUsed) varNames
    isUsed nm = isFree nm || nm==idn || (nm `elem` pu)

numBindsUsed :: Rul Symb -> Int
numBindsUsed (RUL _ arg bod) =
    go (fromIntegral arg) bod
  where
    go :: Int -> Bod Symb -> Int
    go top = \case
        BVAR{}   -> top
        BCNS{}   -> top
        BBAD{}   -> top
        BLET b x -> max (go (top+1) b) (go (top+1) x)
        BAPP f x -> max (go top f) (go top x)

isCode :: Nat -> Val a -> Bool
isCode nex (NAT n)                 = n<nex
isCode _   (NAT 0 `APP` _ `APP` _) = True
isCode _   (NAT 1 `APP` _ `APP` _) = True
isCode _   (NAT 2 `APP` _)         = True
isCode _   _                       = False

goBod :: Set Symb -> (Symb -> Bool) -> [Symb] -> Nat -> Bod Symb -> XBod
goBod pu isGlo names nex = \case
    BCNS v   -> case v of
                    APP{}            -> XCNS(resugarVal pu v)
                    _ | isCode nex v -> XRAW(resugarVal pu v)
                    _                -> XCNS(resugarVal pu v)
    BBAD v   -> case v of
                    _ | isCode nex v -> XBAD(resugarVal pu v)
                    APP{}            -> XBAD(resugarVal pu v)
                    _                -> XRAW(resugarVal pu v)
    BAPP f x -> XAPP (go nex f) (go nex x)
    BLET v k -> XLET (names!!(fromIntegral nex)) (go(nex+1) v) (go(nex+1) k)
    BVAR v   -> if v >= nex
                then XBAD (XVNAT 0 `XVAPP` XVNAT (fromIntegral v))
                else XVAR (names!!(fromIntegral v))
  where
    go = goBod pu isGlo names

varNames :: [Symb]
varNames = fmap (utf8Nat . pack) strs
  where
    alphabet = ['a'..'z']
    strs = fmap singleton alphabet <> do
             var    <- strs
             letter <- alphabet
             pure (var <> [letter])
