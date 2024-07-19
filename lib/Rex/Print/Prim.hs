-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

{- | This module handles wide-mode rex printing. -}

{-
    TODO: Move these all back into Rex.Print and just don't export them.

    TODO: Simply assume that all leaves are valid and print them without
    handling edge-cases.  The code above this should enforce.

    Alternatively, explicitly transform @Rex -> ValidRex@ before printing.
-}

module Rex.Print.Prim
    ( RexBuilder(..), rbRun, rbText, rbChar, rbIntercalate
    , RexColorScheme(..)
    , RexColor
    , rexLine
    , rexLineBuilder
    , cRune
    , cOpen
    , cText
    , cNest
    , welcomeComment
    , errorComment
    )
where

import PlunderPrelude
import Rex.Types
import Rex.Mechanism (wordy)

import qualified Text.Builder as TB


-- Rex Builders ----------------------------------------------------------------

{-|
   This is just a text builder, except that have our own count of
   the width.  Our count does not include the escape characters that
   are used for color output.
-}
data RexBuilder = RB
    { width :: !Int
    , chars :: !TB.Builder
    }

instance Semigroup RexBuilder where
    (<>) a b = RB (a.width + b.width) (a.chars <> b.chars)

instance Monoid RexBuilder where
    mempty = RB 0 mempty

instance IsString RexBuilder where
    fromString str = rbText (pack str)

instance Show RexBuilder where show = show . rbRun

rbText :: Text -> RexBuilder
rbText txt = RB (length txt) (TB.text txt)

rbChar :: Char -> RexBuilder
rbChar chr = RB 1 (TB.char chr)

rbRun :: RexBuilder -> Text
rbRun rb = TB.run rb.chars

rbIntercalate :: Text -> [RexBuilder] -> RexBuilder
rbIntercalate _ []      = mempty
rbIntercalate _ [x]     = x
rbIntercalate t (x:y:z) = x <> rbText t <> rbIntercalate t (y:z)

--------------------------------------------------------------------------------

data RexColorScheme
    = NoColors
    | BoldColors
  deriving (Eq, Ord, Show)

-- TODO What's the difference between TB.Builder and TextBuilder?
data RexColoring = RC
    { rcOpen :: Text -> RexBuilder
    , rcRune :: Text -> RexBuilder
    , rcText :: RexBuilder -> RexBuilder
    , rcBare :: RexBuilder -> RexBuilder
    , rcNest :: RexBuilder -> RexBuilder
    , rcErro :: RexBuilder -> RexBuilder
    }

noColoring :: RexColoring
noColoring = RC
    { rcOpen = rbText
    , rcRune = rbText
    , rcText = id
    , rcBare = id
    , rcNest = id
    , rcErro = id
    }

{-
   boldColoring takes care not to increase the (.width) of text by
   coloring it.
-}
boldColoring :: RexColoring
boldColoring = RC
    { rcOpen = boldYellow . rbText
    , rcRune = \case
        rt | lightRune rt -> boldYellow (rbText rt)
        rt                -> yellow (rbText rt)
    , rcText = green
    , rcBare = id
    , rcNest = boldMagenta
    , rcErro = boldMagenta -- TODO Red
    }
  where
    esc code = "\x001b[" <> code <> "m"

    green t       = RB t.width (esc "32"   <> t.chars <> esc "0")
    yellow t      = RB t.width (esc "33"   <> t.chars <> esc "0")
    boldYellow  t = RB t.width (esc "33;1" <> t.chars <> esc "0")
    boldMagenta t = RB t.width (esc "35;1" <> t.chars <> esc "0")

    lightRune "-" = True
    lightRune "`" = True
    lightRune "." = True
    lightRune _   = False

rc :: RexColorScheme -> RexColoring
rc NoColors   = noColoring
rc BoldColors = boldColoring

type RexColor = (?rexColors :: RexColorScheme)

cNest :: RexColor => RexBuilder -> RexBuilder
cNest = rcNest where RC{..} = rc ?rexColors

cRune :: RexColor => Text -> RexBuilder
cRune rune = rcRune rune where RC{..} = rc ?rexColors

cOpen :: RexColor => Text -> RexBuilder
cOpen rune = rcOpen rune where RC{..} = rc ?rexColors

cText :: RexColor => RexBuilder -> RexBuilder
cText = rcText where RC{..} = rc ?rexColors

cErr :: RexColor => RexBuilder -> RexBuilder
cErr = rcErro where RC{..} = rc ?rexColors

cBare :: RexColor => RexBuilder -> RexBuilder
cBare = rcBare where RC{..} = rc ?rexColors

welcomeComment :: RexColor => Text -> Text
welcomeComment = rbRun . cText . rbText

errorComment :: RexColor => Text -> Text
errorComment = rbRun . cErr . rbText

-- Expression ------------------------------------------------------------------

{-
    This presumes that the input is not a LINE and that it is quote-safe.
-}
wideLeaf :: RexColor => TextShape -> Text -> RexBuilder
wideLeaf = curry \case
    (WORD, t) -> cBare (rbText t)
    (TEXT, t) ->
        case quoteNeeded t of
            Nothing -> cText (rbChar '{' <>             rbText t <> rbChar '}')
            Just c  -> cText (rbChar '}' <> rbChar c <> rbText t <> rbChar c)
    (LINE, _) -> error "Impossible"

runeChars :: [Char]
runeChars = "$!#%&*+,-./:<=>?@\\^`'\"|~"

wordChars :: [Char]
wordChars = ("_" <> ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9'])

quoteNeeded :: Text -> Maybe Char
quoteNeeded t = do
    guard $ not $ matching 1 $ toList t <> ['}']
    if length t < 8 && not ('_' `elem` t) && not (' ' `elem` t) then
        pure '_'
    else
        -- If we don't have a good way to print it (it uses all
        -- characters, just print it correctly).
        --
        -- (The fully general way would be to force this to be a
        -- line-string, but this is an extremely rare edge-case so we'll
        -- punt for now.
        headMay $ filter (not . (`elem` t))
                $ ("|#" <> runeChars <> wordChars :: [Char])
  where
    matching :: Int -> [Char] -> Bool
    matching 0 []       = True
    matching 0 _        = False
    matching _ []       = False
    matching i ('{':cs) = matching (i+1) cs
    matching i ('}':cs) = matching (i-1) cs
    matching i (_  :cs) = matching i     cs

isName :: Text -> Bool
isName t = not (null t) && all ((`elem` wordy) . fromIntegral . fromEnum) t


{-
   TOOD Test this.
-}
fixWide :: TextShape -> Text -> Maybe Rex
fixWide LINE t            = Just (T TEXT t Nothing)
fixWide TEXT _            = Nothing
fixWide WORD t | isName t = Nothing
fixWide WORD t            = Just (T TEXT t Nothing)

isShut :: Rex -> Bool
isShut (N PREF  _ _ _)  = True
isShut (N SHUT   _ _ _) = True
isShut (C v)            = absurd v
isShut _                = False

rexLine :: RexColor => Rex -> Text
rexLine = rbRun . rexLineBuilder

{-
  TODO Some combinations of shut forms do not need to be wrapped.
-}
wrapRex :: RexColor => Rex -> RexBuilder
wrapRex x | isShut x = cNest "(" <> rexLineBuilder x <> cNest ")"
wrapRex x            = rexLineBuilder x

wrapHeir :: RexColor => Rex -> RexBuilder
wrapHeir x@(N PREF _ _ _) = wrapRex x
wrapHeir x                = rexLineBuilder x

barNest :: RexColor => [RexBuilder] -> RexBuilder
barNest [x] = parens [rbText "|", x]
barNest xs  = parens xs

parens :: RexColor => [RexBuilder] -> RexBuilder
parens xs = cNest "(" <> intercalate " " xs <> cNest ")"

rexLineBuilder :: RexColor => Rex -> RexBuilder
rexLineBuilder = go
 where

  go :: Rex -> RexBuilder
  go = \case
    T s t Nothing         -> case fixWide s t of
                                 Nothing -> wideLeaf s t
                                 Just rx -> go rx
    T s t (Just k)        -> go (T s t Nothing) <> wrapHeir k
    N OPEN  r ps k        -> go (N NEST r ps k)
    N s     r ps (Just k) -> wrapRex (N s r ps Nothing) <> wrapHeir k
    C c                   -> absurd c
    N s     r ps Nothing  ->
      case s of
        PREF -> cRune r <> wrapRex (unsafeHead ps)
        SHUT -> intercalate (cRune r) (wrapRex <$> ps)
        INFX -> parens $ intersperse (cRune r) (infixApp <$> ps)
        NEST -> case r of
          "|" -> barNest (go <$> ps)
          "," -> brackets (go <$> ps)
          _   -> parens (cRune r : fmap go ps)

  brackets :: [RexBuilder] -> RexBuilder
  brackets xs = cNest "[" <> intercalate " " xs <> cNest "]"

  infixApp :: Rex -> RexBuilder
  infixApp x@T{}          = go x
  infixApp x@(C{})        = go x
  infixApp x@(N t r ps k) =
    if isApp
    then rbIntercalate " " (go <$> params)
    else go x
   where
    params = (ps <> toList k)
    isApp = case (params, r, t) of
              ([],     _,   _   ) -> False
              (_:_:_,  "|", NEST) -> True
              _                   -> False
