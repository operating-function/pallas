-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall                 #-}
{-# OPTIONS_GHC -Wno-orphans          #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# LANGUAGE NoFieldSelectors         #-}

{-
    This file implements the Rex "policy", it takes the raw trees produced
    by Rex.Mechanism and converts them to Rex.

    In particular, this assigns meaning to juxtaposition and nesting.

        3+4 is (3 + 4) is (+ 3 4), etc.

    This process is also technically total, we always produce a result.
    However, if the input is malformed, the output will contain nonsense.

    Unexpected characters are interpreted as runes, nested forms
    auto-close, non-terminated strings lose their last character, etc.

    This "policy" is meant to be used in conjunction with a "validation pass"
    like the one supplied by `Rex.Validate`.

    Separating these two passes makes a hyper-minimal non-validating
    version possible, which is useful for creating a tiny bootstrapping
    seed.
-}

module Rex.Policy (treesRex, rexStep, Block(..)) where

import PlunderPrelude hiding (many, head, last)
import Rex.Mechanism
import Rex.Validate

import Control.Monad.Writer (execWriter)
import Prelude              (foldl1)
import Rex.Types            (GRex(..), Rex, RuneShape(..))

import qualified Rex.Types as R


-- Types -----------------------------------------------------------------------

data Block = BLOCK
    { lineNum :: Int
    , lines   :: [Line]
    , errors  :: [Text]
    , rex     :: Rex
    }

data SubLayout a
    = PREFX ByteString [Frag a]
    | INFIX (NonEmpty a) ByteString (SubLayout a)
    | SEQUE [a]


-- Policy ----------------------------------------------------------------------

subLayout :: [Frag a] -> SubLayout a
subLayout = \case
    []            -> SEQUE []
    WOLF r _ : fs -> PREFX r fs
    LAMB x   : fs -> noPrefix x hed rest where (rest, hed) = lambs fs
  where
    noPrefix :: a -> [a] -> Maybe (ByteString, [Frag a]) -> SubLayout a
    noPrefix x xs Nothing            = SEQUE (x:xs)
    noPrefix x xs (Just (run, more)) = INFIX (x :| xs) run (subLayout more)

    lambs :: [Frag a] -> (Maybe (ByteString, [Frag a]), [a])
    lambs (LAMB x   : more) = (x:) <$> lambs more
    lambs (WOLF r _ : more) = (Just (r,more), [])
    lambs []                = (Nothing, [])

tightPolicy :: SubLayout Elem -> Rex
tightPolicy = \case
    PREFX r xs    -> N PREF (decodeUtf8 r) [tightPolicy (subLayout xs)] Nothing
    INFIX es r xs -> eatInfix r [heirSeq es] xs
    SEQUE []      -> error "impossible: empty tight sequence"
    SEQUE (x:xs)  -> heirSeq (x :| xs)
  where
    heirSeq = foldl1 rexAddCont . fmap elemRex

    ifix rune acc = N SHUT (decodeUtf8 rune) (reverse acc) Nothing

    eatInfix rune acc end@SEQUE{}     = ifix rune (tightPolicy end : acc)
    eatInfix _ _ PREFX{}              = error "impossible: double tight rune"
    eatInfix rune acc (INFIX es r xs) =
        if r==rune then eatInfix rune (heirSeq es : acc) xs
                   else eatInfix r [ifix rune (heirSeq es : acc)] xs

clumpRex :: Clump -> Rex
clumpRex = tightPolicy . subLayout . fmap spanFrag . toList . (.cs.x)



-- Parens ----------------------------------------------------------------------

parenPolicy :: Elem -> SubLayout Clump -> Rex
parenPolicy e = \case
    PREFX r xs    -> prefixed r [] xs
    INFIX es r xs -> let rx = parenSeq (toList es) in infixed r [rx] xs
    SEQUE xs      -> parenSeq xs
  where
    parenSeq [c] = clumpRex c
    parenSeq cs  = N NEST "|" (clumpRex <$> toList cs) Nothing

    mkInfix rune acc = N R.INFX (decodeUtf8 rune) (reverse acc) Nothing

    prefixed rune acc = \case
        WOLF rx _ : m -> prefixed rune (prefixed rx [] m : acc) []
        LAMB cl   : m -> prefixed rune (clumpRex cl : acc) m
        []            -> N NEST (decodeUtf8 rune) (reverse acc) Nothing

    infixed rune acc = \case
        end@SEQUE{}   -> mkInfix rune (parenPolicy e end : acc)
        PREFX r xs    -> mkInfix rune (prefixed r [] xs : acc)
        INFIX es r xs -> let rx = parenSeq (toList es) in
                         if r==rune
                         then infixed rune (rx:acc) xs
                         else infixed r [mkInfix rune (rx:acc)] xs


-- Policy ----------------------------------------------------------------------

rexAddCont :: R.GRex Void -> R.GRex Void -> R.GRex Void
rexAddCont (R.T s t Nothing) c  = R.T s t (Just c)
rexAddCont (R.T s t (Just k)) c = R.T s t (Just $ rexAddCont k c)
rexAddCont (N m r x Nothing) c  = N m r x (Just c)
rexAddCont (N m r x (Just k)) c = N m r x (Just $ rexAddCont k c)
rexAddCont (R.C x) _            = absurd x

elemRex :: Elem -> Rex
elemRex e = case e.x of
   WORD        -> T R.WORD (decodeUtf8 $ spanBar e) Nothing
   LINE x      -> lineRex (spanBar e) (spanBar <$> reverse x)
   CORD QUOTED -> T R.TEXT (quotData e) Nothing
   CORD _      -> T R.TEXT (textData e) Nothing
   FAIL        -> elemRex (e{x=RUNE})
   PARA cs     -> parenPolicy e $ subLayout $ snd . clumpFrag <$> cs
   BRAK cs     -> N NEST "," (clumpRex <$> onlyLambs cs) Nothing
   _           -> error "impossible: already handled"
  where
    onlyLambs  = catMaybes . fmap (getLamb . snd . clumpFrag)
    getLamb    = \case LAMB x -> Just x; _ -> Nothing
    textData s = let b = spanBar s in decodeUtf8 $ drop 1 $ take (length b-1) b
    quotData s = let b = spanBar s in decodeUtf8 $ drop 2 $ take (length b-1) b

    lineRex t ts = T R.LINE (decodeUtf8 $ drop 2 t) (more ts)
      where more = \case { [] -> Nothing ; m:ms -> Just (lineRex m ms) }

treesRex :: [Tree] -> Rex
treesRex = \case
    []  -> error "empty block"
    [t] -> go t
    ts  -> go (NODE "|" ts Nothing)
  where
    go (LEAF c)              = clumpRex c
    go (NODE rune sons heir) = N R.OPEN (decodeUtf8 rune) sons' heir'
        where sons' = go <$> sons
              heir' = go <$> heir

rexStep :: BlockState -> Maybe ByteString -> (BlockState, [Block])
rexStep st mInp = fmap treesRexChecked <$> treeStep st mInp

-- TODO: actually collect the input lines.  This needs to be threaded
-- all the way through starting with `blockStep`.
treesRexChecked :: (Int, [Tree]) -> Block
treesRexChecked (lineNum, ts) =
    BLOCK { lineNum, lines=[], errors, rex=(treesRex ts) }
  where
    errors = execWriter (for_ ts checkTree)
