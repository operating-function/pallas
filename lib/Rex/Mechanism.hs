-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall -Werror #-}

{-
    This file concerns itself with basic lexing and interpreting
    indentation-based layout.  It understands nested structures ([],
    (), and {}, and juxtaposition), but doesn't interpret those.

    This process is *total*.  We emit unexpected characters as `FAIL`
    tokens.  The parsing otherwise has no failure cases.
-}

module Rex.Mechanism
    ( Line(..), Span(..), Lexi(..), Elem, Clump(..), Tree(..), Frag(..)
    , StrType(..)
    , runic, wordy
    , clumpFrag, spanFrag, spanBar
    , BlockState, blockState, treeStep
    )
where

import PlunderPrelude hiding (many, head, last)

import Data.List.NonEmpty (head, last)
import Prelude            (foldl1)

import qualified Data.ByteString as BS


-- Types -----------------------------------------------------------------------

data Line = LN
    { fil :: !FilePath
    , num :: !Int
    , byt :: !ByteString
    }
  deriving (Eq, Ord, Show)

data Span a = S
    { lin :: !Line -- line
    , off :: !Int  -- offset
    , end :: !Int  -- end
    , x :: a
    }
  deriving (Eq, Ord, Show, Functor)

data StrType = CURLY | QUOTED
  deriving (Eq, Ord, Show)

data Lexi a
    = RUNE
    | WORD
    | WYTE
    | SEMI
    | LINE [Span ()]
    | CORD StrType
    | PARA a
    | BRAK a
    | FAIL
    | TERM
  deriving (Eq, Ord, Show, Functor)

newtype Lexeme = L { t :: Span (Lexi [Lexeme]) }
  deriving (Eq, Ord, Show)

type Elem = Span (Lexi [Clump])

newtype Clump = C { cs :: Span (NonEmpty Elem) }


-- Do Basic Lexing + Nesting ---------------------------------------------------

lex :: Line -> [Lexeme]
lex ln@(LN _ _ b) = many 0 0
  where
    wid = length b

    x off end tok = L (S ln off end tok)

    many i ctx = l : (if l.t.x == TERM then [] else many l.t.end ctx)
                   where l = one i ctx

    nest cn ctx i = x i (unsafeLast ts).t.end (cn ts)
                      where ts = many (i+1) ctx

    eat o f = fromMaybe wid $ fmap (+o) $ BS.findIndex (not . f) $ drop o b

    curl :: Int -> Int -> Int
    curl i 0 = i
    curl i d = maybe i (\c -> curl (i+1) (d + curlStep c)) (b BS.!? i)
                 where curlStep = \case { 123 -> 1; 125 -> (-1); _ -> 0 }

    str i = x i (min wid (succ $ eat (i+1) (/= 34))) (CORD CURLY)

    quote i = let c = fromMaybe 0 (b BS.!? succ i) in
              if c==0 || c==32 then x i wid (LINE[]) else
              x i (min wid $ succ $ eat (i+2) (/= c)) (CORD QUOTED)

    one i ctx = case fromMaybe 0 (b BS.!? i) of
                    40  {- ( -}      -> nest PARA 41  i  {- ) -}
                    91  {- [ -}      -> nest BRAK 93  i  {- ] -}
                    34  {- " -}      -> str i
                    125 {- } -}      -> quote i
                    123 {- { -}      -> x i (curl (i+1) 1)         (CORD CURLY)
                    0   {- ø -}      -> x i wid                    TERM
                    59  {- ; -}      -> x i wid                    SEMI
                    32  {-   -}      -> x i (eat i (== 32))        WYTE
                    c | elem c wordy -> x i (eat i (`elem` wordy)) WORD
                    c | elem c runic -> x i (eat i (`elem` runic)) RUNE
                    c | ctx==c       -> x i (i+1)                  TERM
                    _                -> x i (i+1)                  FAIL

runic, wordy :: ByteString
wordy = encodeUtf8 (pack ("_" <> ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']))
runic = "$!#%&*+,-./:<=>?@\\^`'|~"


-- Merge Multi-Line Strings ----------------------------------------------------

multiLine :: [Lexeme] -> [Lexeme]
multiLine = \case
    ( ( L ln@S{x=LINE acc}
      : L S{x=TERM}
      : (getMatch ln -> Just (bs, more))
      )) -> multiLine ( L ln{x=(LINE (bs:acc))} : more )
    []   -> []
    l:ls -> l : multiLine ls
  where
    getMatch s1 toks = do
        (s2, more) <- case toks of
            L s@S{x=LINE _}               : m -> Just (s,m)
            L S{x=WYTE} : L s@S{x=LINE _} : m -> Just (s,m)
            _                                 -> Nothing
        guard ((s1.off == s2.off))
        pure (const () <$> s2, more)


-- Clump Juxtaposed Tokens -----------------------------------------------------

mkClump :: NonEmpty Elem -> Clump
mkClump x = C S{x=x, lin=(head x).lin, off=(head x).off, end=(last x).end}

lexemeElem :: Lexeme -> Elem
lexemeElem (L (S l o e x)) = S l o e (clump <$> x)

clump :: [Lexeme] -> [Clump]
clump = go [] . fmap lexemeElem
  where
    go :: [Elem] -> [Elem] -> [Clump]
    go []     []               = []
    go []     (i:is) | isSpc i = go [] is
    go (t:ts) []               = mkClump (reverse (t:|ts)) : []
    go (t:ts) is | isEnd is    = mkClump (reverse (t:|ts)) : go [] is
    go ts     (i:is)           = go (i:ts) is

    isSpc S{x=WYTE} = True
    isSpc S{x=SEMI} = True
    isSpc S{x=TERM} = True
    isSpc _         = False

    isEnd ((isSpc->True) : _)                 = True
    isEnd (S{x=RUNE}     : [])                = True
    isEnd (S{x=RUNE}     : (isSpc->True) : _) = True
    isEnd _                                   = False


-- Layout Engine (Understand Structure Implied by Indentation) -----------------

data Tree = LEAF !Clump | NODE !ByteString ![Tree] !(Maybe Tree)

data Frag a = WOLF ByteString a | LAMB a

data Item = I ByteString [Tree] (Maybe Tree)

fTree :: Frag Clump -> Tree
fTree (LAMB c)   = LEAF c
fTree (WOLF b _) = NODE b [] Nothing

iTree :: Item -> Tree
iTree (I t x k) = NODE t (reverse x) k

close :: Int -> [(Int, Item)] -> [(Int, Item)]
close _ []                   = []
close p (i:is)  | p >= fst i = (i:is)
close p (i:j:k)              = let ij = merge (iTree <$> i) j in close p (ij:k)
close _ _                    = error "indent too small.  Bug in block splitter"

pushOnto :: [(Int, Item)] -> (Int, Frag Clump) -> [(Int, Item)]
pushOnto stack (fp,f) =
    let stc = close fp stack
    in case (f, stc) of
        (LAMB _,   [])   -> error "impossible; this case is already handled"
        (WOLF r _, is)   -> ((fp, I r [] Nothing) : is)
        (LAMB _,   i:is) -> (merge (fp, fTree f) i : is)

merge :: (Int, Tree) -> (Int, Item) -> (Int, Item)
merge (rp,r) (ip,i) =
    case (compare rp ip, i) of
        (LT , _              ) -> error "impossible merge"
        (_  , I t cs (Just k)) -> (ip, I t (k:cs) (Just r))
        (EQ , I t cs Nothing ) -> (ip, I t cs (Just r))
        (GT , I t cs Nothing ) -> (ip, I t (r:cs) Nothing)

layout :: [(Int, Frag Clump)] -> [Tree]
layout = \case
    []                 -> []
    (_, f@LAMB{}) : fs -> fTree f : layout fs
    (p, WOLF r _) : fs -> [iTree $ snd $ foldl1 forceMerge pushed]
                            where pushed = pushAll (p, I r [] Nothing) fs
  where
    pushAll :: (Int, Item) -> [(Int, Frag Clump)] -> [(Int, Item)]
    pushAll pf fs = foldl' pushOnto [pf] fs

    forceMerge :: (Int, Item) -> (Int, Item) -> (Int, Item)
    forceMerge a b = merge (iTree <$> a) b


-- Block Splitter --------------------------------------------------------------

data LineCat = VOID | NOTE | OPEN Int | TEXT Int | SING Int
  deriving (Eq, Ord, Show)

data BlockBuffer
    = WOODS
    | TEXTY { ls :: ![[Lexeme]], deep :: !Int }
    | BLOCK { ls :: ![[Lexeme]], deep :: !Int }

data BlockState = BS FilePath Int BlockBuffer

lineCat :: [Lexeme] -> LineCat
lineCat  = \case
    []                                 -> VOID
    L S{x=TERM{}}                 : _  -> VOID
    L S{x=SEMI{}}                 : _  -> NOTE
    L s@S{x=(LINE _)}             : _  -> TEXT s.off
    L s@S{x=RUNE{}} : L S{x=TERM} : _  -> OPEN (s.end - 1)
    L s@S{x=RUNE{}} : L S{x=WYTE} : _  -> OPEN (s.end - 1)
    L s@S{x=RUNE{}} : L S{x=SEMI} : _  -> OPEN (s.end - 1)
    L(S{x=WYTE{}})                : ls -> lineCat ls
    L s                           : _  -> SING s.off

blockStep
    :: BlockState
    -> Maybe [Lexeme]
    -> (BlockState, [[[Lexeme]]])
blockStep (BS fn lno st) inp = case inp of
    Nothing ->
        (BS fn lno WOODS,) $
        case st of
            WOODS     -> []
            TEXTY{ls} -> [reverse ls]
            BLOCK{ls} -> [reverse ls]

    Just l ->
        let c = lineCat l in
        let x s o = (BS fn (lno+1) s, o) in
        case (st, c) of
            (WOODS, TEXT d  ) -> x (TEXTY [l] d) []
            (WOODS, OPEN d  ) -> x (BLOCK [l] d) []
            (WOODS, VOID    ) -> x WOODS []
            (WOODS, NOTE    ) -> x WOODS []
            (WOODS, SING{}  ) -> x WOODS [[l]]

            (TEXTY{..}, TEXT d) | d==deep -> x TEXTY{ls=l:ls,..} []

            (BLOCK{ls,deep}, NOTE  )           -> x BLOCK{deep,ls=(l:ls)} []
            (BLOCK{ls,deep}, TEXT d) | d>=deep -> x BLOCK{deep,ls=(l:ls)} []
            (BLOCK{ls,deep}, OPEN d) | d>=deep -> x BLOCK{deep,ls=(l:ls)} []
            (BLOCK{ls,deep}, SING d) | d>=deep -> x BLOCK{deep,ls=(l:ls)} []

            (BLOCK{ls},_) -> (reverse ls :) <$> blockStep (BS fn lno WOODS) inp
            (TEXTY{ls},_) -> (reverse ls :) <$> blockStep (BS fn lno WOODS) inp


-- Take Lines, Spit out Trees --------------------------------------------------

spanFrag :: Span (Lexi a) -> Frag (Span (Lexi a))
spanFrag s@S{x=RUNE} = WOLF (spanBar s) s
spanFrag s           = LAMB s

spanBar :: Span a -> ByteString
spanBar s = take (s.end - s.off) $ drop s.off s.lin.byt

clumpFrag :: Clump -> (Int, Frag Clump)
clumpFrag c@(C cs) =
    case cs.x of
        _ :| (_:_) -> (cs.off, LAMB c)
        e :| []    -> case spanFrag e of
                          WOLF rune _ -> (cs.off + length rune - 1, WOLF rune c)
                          LAMB _      -> (cs.off, LAMB c)

blockState :: FilePath -> BlockState
blockState fn = BS fn 1 WOODS

treeStep :: BlockState -> Maybe ByteString -> (BlockState, [(Int, [Tree])])
treeStep st@(BS fil num _) mInp = (st2, treeOut)
  where
    (st2, out) = blockStep st (mInp <&> \byt -> lex (LN fil num byt))
    treeOut = out <&> \bls ->
                  ( (unsafeHead (unsafeHead bls)).t.lin.num
                  , layout $ fmap clumpFrag $ clump $ multiLine $ concat bls
                  )
