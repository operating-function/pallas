-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

{- |

    Produces a single Rex value by reducing a list of lexer `Frag`s.

    TODO Produce better errors on merge failure.

        We will need to associate a SrcSpan with every Rex in order to
        do this.

    TODO Alternatively, find some way to eliminate the one remaining
    edge-cases that requires this code to have an error-path at all:

      """ foo
            (bar)
          "zaz"

    What should the layout algorithm do with the above?

-}

module Rex.Parser (parseBlock) where

import PlunderPrelude
import Rex.Lexer

import Prelude (foldl1)

import qualified Rex.Types as R


-- Basic Concepts --------------------------------------------------------------

data Elem
    = I Text [R.Rex] (Maybe R.Rex)
    | T Bool Text    (Maybe R.Rex)
  deriving (Show)

iRex :: Elem -> R.Rex
iRex (I t x k) = R.N R.OPEN t (reverse x) k
iRex (T f t k) = R.T (if f then R.THIC_LINE else R.THIN_LINE) t k

fRex :: Frag -> R.Rex
fRex (RUNE rune)    = R.N R.OPEN rune [] Nothing
fRex (FORM wide)    = formToRex wide
fRex (LINE True t)  = R.T R.THIC_LINE t Nothing
fRex (LINE False t) = R.T R.THIN_LINE t Nothing


-- Converting Forms into Runic Trees -------------------------------------------

formToRex :: Form -> R.Rex
formToRex = form
 where
  form :: Form -> R.Rex
  form (BEFO ru bod) = rn R.SHUT_PREFIX ru [form bod]
  form (SHIP i)      = itmz i
  form (SHIN ru ps)  = rn R.SHUT_INFIX ru (itmz <$> ps)

  nest :: Nest -> R.Rex
  nest (WRAPD f)    = form f
  nest (PREFX r fs) = rn R.NEST_PREFIX r (form <$> fs)
  nest (PAREN ns)   = goApp ns
  nest (INFIX r fs) = rn R.NEST_INFIX  r (goApp <$> fs)

  goApp :: [Form] -> R.Rex
  goApp = form . \case [x] -> x
                       xs  -> SHIP (NEST (PREFX "|" xs) :| [])

  itmz :: Itmz -> R.Rex
  itmz (i :| [])   = item i
  itmz (i :| k:ks) = rexAddCont (item i) (itmz (k:|ks))

  item :: Item -> R.Rex
  item (LEAF (N t))       = R.T R.BARE_WORD t Nothing
  item (LEAF (C THICK t)) = R.T R.THIC_CORD t Nothing
  item (LEAF (C THIN t))  = R.T R.THIN_CORD t Nothing
  item (LEAF (C CURL t))  = R.T R.CURL_CORD t Nothing
  item (NEST n)           = nest n

  rn m r cs = R.N m r cs Nothing -- "Rune node"

  rexAddCont :: R.GRex Void -> R.GRex Void -> R.GRex Void
  rexAddCont (R.T s t Nothing) c    = R.T s t (Just c)
  rexAddCont (R.T s t (Just k)) c   = R.T s t (Just $ rexAddCont k c)
  rexAddCont (R.N m r x Nothing) c  = R.N m r x (Just c)
  rexAddCont (R.N m r x (Just k)) c = R.N m r x (Just $ rexAddCont k c)
  rexAddCont (R.C x) _              = absurd x


-- Reducing the Stack ----------------------------------------------------------

impossible :: String -> a
impossible str = error ("Impossible: " <> str)

-- | Add a rex value into an Elem, either as a parameter or as a
-- continuation, depending on depth.
--
-- This cannot merge a less-deeply nested item into a more-deeply nested
-- one, the caller must maintain that invariant.
--
-- It is invalid for line-strings to have child-nodes, that's the only
-- input that we handle but reject.
--
--     """ Hello, hello
--         """ Yeah, that's right?
--         """ What're you going to do about it?
--
-- There is a situation where we are adding a continuation to a form
-- that already has a continuation, for example:
--
--     | x
--     y
--     z
--
-- This can only happen with closed forms, so we just choose the last
-- same-depth form as the continuation and interpret all of the
-- same-depth forms before that as arguments:
--
--     | x y
--     z
--
merge :: (Int, R.Rex) -> (Int, Elem) -> Either Text (Int, Elem)
merge (rp,r) (ip,i) =
  case (compare rp ip, i) of
    (LT , _              ) -> impossible "invalid merge"
    (_  , T _  _ Just{}  ) -> Left "Line strings cannot have child nodes."
    (GT , T _  _ Nothing ) -> Left "Line strings cannot have child nodes."
    (EQ , T th t Nothing ) -> pure (ip, T th t (Just r))
    (_  , I t cs (Just k)) -> pure (ip, I t (k:cs) (Just r))
    (EQ , I t cs Nothing ) -> pure (ip, I t cs (Just r))
    (GT , I t cs Nothing ) -> pure (ip, I t (r:cs) Nothing)


{-
    TODO: Why can't line-strings have child nodes?  Can't we just have
    that end the line-string and push another argument onto the last
    open rune?
-}

-- | Reduce the stack until singleton or top item has position <= p.
--
-- If the first rune in the file is indented less deeply than subsequent
-- runes, that cannot be closed out, so we reject the input.
close :: Int -> Frag -> [(Int, Elem)] -> Either Text [(Int, Elem)]
close _ _ []                   = pure []
close p _ (i:is)  | p >= fst i = pure (i:is)
close p f (i:j:k)              = do ij <- merge (iRex <$> i) j; close p f (ij:k)
close _ _ _                    = error badBlock
  where
    badBlock = "Internal Error: item indented too little for block"


{-
    Pushes a fragment onto the item stack.

    -   Runes are simply pushed onto the stack as empty items.

    -   Forms are merged into the head of the stack.
-}
pushOnto :: [(Int, Elem)] -> (Int, Frag) -> Either Text [(Int, Elem)]
pushOnto stack (fp,f) = do
  stc <- close fp f stack
  case (f, stc) of
    (FORM _, [])   -> impossible "Just-Form case already handled in `rush`"
    (LINE{}, [])   -> impossible "Just-Page case already handled in `rush`"
    (RUNE r, is)   -> pure ((fp, I r [] Nothing) : is)
    (LINE s l, is) -> pure ((fp, T s l Nothing) : is)
    (FORM _, i:is) -> (:is) <$> merge (fp, fRex f) i


-- | Given a list of fragments in parse order, produce a Rex.
rush :: [(Int, Frag)] -> Either Text (Maybe R.Rex)
rush = \case
  []                  -> error "impossible: rush given an empty list"
  (p, RUNE r)    : fs -> fmap (Just . iRex . snd . foldl1 forceMerge)
                              (pushAll (p, I r [] Nothing) fs)
  (p, LINE th l) : fs -> fmap (Just . iRex . snd . foldl1 forceMerge)
                              (pushAll (p, T th l Nothing) fs)
  (_, w@FORM{})  : [] -> pure $ Just $ fRex w
  (_, FORM{})    : _  -> error oneForm

 where
  oneForm = "Blocks starting with closed forms may only contain one form."

  pushAll :: (Int, Elem) -> [(Int, Frag)] -> Either Text [(Int, Elem)]
  pushAll pf fs = foldlM pushOnto [pf] fs

  forceMerge :: (Int, Elem) -> (Int, Elem) -> (Int, Elem)
  forceMerge a b = either (impossible . unpack) id (merge (iRex <$> a) b)


-- Entry Point -----------------------------------------------------------------

{-
    Given a block of parsed lines, use an indentation-based layout
    algorithm to combine these lines into one rex tree.

    This algorithm makes several assumptions about its input and will
    crash if those assumptions are not respected.

    Those rules must be enforced by the code that breaks lines into
    blocks.

    -   A block that does not start with an open-form rune must contain
        only one line.  (If a block starts with a closed form, the block
        is just one line)

    -   The first form on the first line defines the minimum indentation
        level.  No form may be indented less than the first form.
        (If a line starts with a form that is indented less than the
        first form in the block, it should break the block and be treated
        as the first line of another block).

    TODO: Integrate the block-splitter into this code so that these are
    internal invariants, not external assumptions.
-}
parseBlock :: [[(Int, Frag)]] -> Either Text (Maybe R.Rex)
parseBlock = \case
    [] : fs -> parseBlock fs
    [ln]    -> lineHack ln
    fs      -> rush (concat fs)
  where
    lineHack (a@(_, FORM{}) : b : cs) = rush ((-1, RUNE "|") : a : b : cs)
    lineHack cs                       = rush cs
