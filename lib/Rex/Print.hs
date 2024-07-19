-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

--
--  # :TODO: Handling Printer Edge-Cases
--
--  Applying the following transformations will ensure that printed
--  output always maintains the same tree structure as the given rex node,
--  though we will deviate from the given formatting style in cases
--  where the input cannot be printed as specified.
--
--  -   :TODO: Write a transformation that rewrites string nodes so that they
--      don't contain characters that can't be printed.
--
--      For example, `(THIN_CORD "'")` cannot be printed,
--      but we can coerce it to `(TAPE Nothing).  The very
--      worst case is something like:  `(THIN_CORD "'\"{")`,
--      which can't be printed as a closed form and will need to be
--      coerced to: `(T 0 LINE "'\"{")`.
--
--  -   :TODO: Write a transformation that opens all nodes enclosing an
--      open node:
--
--      For example: `(INFIX "-" [OPEN "|" [] NONE] NONE)`
--      should be coerced to `(OPEN "-" [OPEN "|" [] NONE] NONE)`.
--

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.Print
    ( RexColorScheme(..)
    , RexColor
    , blocksFile
    , rexFile
    , rexFileBuilder
    , rexLine
    , rexLineBuilder
    )
where

import PlunderPrelude
import Rex.Print.Prim
import Rex.Types

import qualified Data.Text as T


-- Expression ------------------------------------------------------------------

{-
    -   {pady} is the extra space required by multi-char runes.
-}
data RunicBlock = RUNIC_BLOCK
    { runeTex :: Text
    , opening :: [RexBuilder]
    , nesting :: [Block]
    , closing :: [RexBuilder]
    , nextOne :: Maybe Block
    }
  deriving (Show)

type Block = (Int, Blocky)

data Blocky
    = PHRASE [RexBuilder] -- Never empty but not worth enforcing.
    | RUNIC RunicBlock
    | LINED Text (Maybe Block)
  deriving (Show)

rexBlock :: RexColor => Rex -> Block
rexBlock rex = case rex of
    C v ->
        absurd v

    T LINE t k ->
        case rexBlock <$> k of
            Nothing   -> (0, LINED t Nothing)
            Just heir -> (fst heir, LINED t (Just heir))

    N OPEN runeTex kids heir ->
        (max ourPad heirPad, RUNIC RUNIC_BLOCK{..})
      where
        nextOne = rexBlock <$> heir
        heirPad = maybe 0 fst nextOne
        ourPad  = length runeTex - 1
        (opening, nesting, closing) = crushEnds kids

    _ ->
        (0, (PHRASE $ singleton $ rexLineBuilder rex))

crushEnds :: RexColor => [Rex] -> ([RexBuilder], [Block], [RexBuilder])
crushEnds kids =
    ( toPhrases initArgs
    , crushMid (reverse middleArgsRv)
    , toPhrases (reverse finalArgsRv)
    )
  where
    (initArgs, more) = span isPhrasic kids

    (finalArgsRv, middleArgsRv) = span isPhrasic (reverse more)

crushMid :: RexColor => [Rex] -> [Block]
crushMid kids =
    case kids of
        []                       -> []
        k:ks | not (isPhrasic k) -> rexBlock k : crushMid ks
        _                        -> separated
  where
    separated = (0, (PHRASE $ toPhrases phrases)) : crushMid rest
    (phrases, rest) = span isPhrasic kids

toPhrases :: RexColor => [Rex] -> [RexBuilder]
toPhrases = \case
    []   -> []
    r:rs -> go (rexLineBuilder r) [] (rexLineBuilder <$> rs)
  where
    go buf acc = \case
        []   -> reverse (buf:acc)
        r:rs -> if (buf.width + 1 + r.width) > 40 then
                    go r (buf:acc) rs
                else
                    go (buf <> " " <> r) acc rs

isPhrasic :: Rex -> Bool
isPhrasic (N OPEN _ _ _) = False
isPhrasic (T LINE _ _)   = False
isPhrasic _              = True

indent :: Int -> RexBuilder
indent depth = rbText (T.replicate depth " ")

blockLines :: RexColor => Int -> Block -> [(Int, RexBuilder)]
blockLines d (pad, val) =
    if pad > d then
        if d == 0
        then blockLines pad   (pad, val)
        else blockLines (d+4) (pad, val)
    else case val of
        PHRASE ps ->
            (d,) <$> toList ps

        LINED t k ->
            (:) (d, mkStr if null t then "}" else "} " <> t)
                (maybe [] (blockLines d) k)

        RUNIC RUNIC_BLOCK{..} ->
            let depth = (d+1) - length runeTex
            in concat
            [ case opening of
                  []   -> [(depth, cRune runeTex)]
                  w:ws -> (:) (depth, cRune runeTex <> " " <> w)
                              (ws <&> \v -> (d+2, v))
            , concat $ reverse
                     $ zip [1,2..] (reverse nesting)
                   <&> \(dd,x) -> blockLines (d+(dd*4)) x
            , closing <&> \v -> (d+2, v)
            , maybe mempty (blockLines d) nextOne
            ]

{-|
    This makes the output more compact by merging lines where possible.

    For example:

        | x
            | y
                | p
          z
        | a

    Becomes:

        | x | y | p
          z
        | a
-}
massage :: [(Int, RexBuilder)] -> [(Int, RexBuilder)]
massage []                 = []
massage [x]                = [x]
massage ((d,x):(e,y):more) =
    let
        diff = e - (d + x.width)
    in if (diff > 0) then
        massage ((d, x <> indent diff <> y) : more)
    else
        (d,x) : massage ((e,y):more)

renderLines :: [(Int, RexBuilder)] -> RexBuilder
renderLines = concat . fmap (\(d,t) -> indent d <> t <> "\n")

blockBuilder :: RexColor => Int -> Block -> RexBuilder
blockBuilder d blk = renderLines $ massage $ blockLines d blk

mkStr :: RexColor => Text -> RexBuilder
mkStr = cText . rbText

rexFileBuilder :: RexColor => Rex -> RexBuilder
rexFileBuilder rex = blockBuilder 0 (rexBlock rex)

rexFile :: RexColor => Rex -> Text
rexFile = rbRun . rexFileBuilder

-- TODO Do we always need the extra newline?
blocksFile :: RexColor => [Rex] -> Text
blocksFile = loop ""
 where
  loop acc []     = rbRun acc
  loop acc [x]    = loop (acc <> rexFileBuilder x) []
  loop acc (x:xs) = loop (acc <> rexFileBuilder x <> "\n") xs
