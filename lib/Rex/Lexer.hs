-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Rex.Lexer
    ( lexLine
    , Frag(..)
    , Form(..)
    , Item(..)
    , Itmz
    , Nest(..)
    , Leaf(..)
    , CordType(..)
    , isRuneChar
    , isNameChar
    , isName
    )
where

import PlunderPrelude       hiding (many, some, try)
import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Monad.Fail (fail)
import Data.List          (nub)

--------------------------------------------------------------------------------

type Parser = Parsec Void Text

data Frag
    = RUNE Text
    | FORM Form
    | PAGE Bool Text
  deriving (Eq, Ord, Show)

data Form
    = BEFO Text Form      --  +()()
    | SHIN Text [Itmz]    --  ()()+()()
    | SHIP Itmz           --  ()()
  deriving (Eq, Ord, Show)

data Item = LEAF Leaf | NEST Nest
  deriving (Eq, Ord, Show)

type Itmz = NonEmpty Item

data Nest
    = INFIX Text [[Form]]
    | PREFX Text [Form]
    | WRAPD Form
    | PAREN [Form]
  deriving (Eq, Ord, Show)

data CordType = THICK | THIN | CURL
  deriving (Eq, Ord, Show)

data Leaf
    = N Text
    | C CordType Text
  deriving (Eq, Ord, Show)

spc :: Parser ()
spc = void $ char ' '

spc0 :: Parser ()
spc0 = void $ many spc

whyt :: Parser ()
whyt = void $ some spc

page :: Parser (Bool, Text)
page = do
    thic <- (string "\"\"\"" $> True)
        <|> (string "'''"    $> False)
    (thic,) <$> takeRest

isNameChar :: Char -> Bool
isNameChar = (`elem` ("_" <> ['a'..'z'] <> ['A'..'Z'] <> ['0'..'9']))

isName :: Text -> Bool
isName t = not (null t) && all isNameChar t

isRuneChar :: Char -> Bool
isRuneChar = (`elem` ("$!#%&*+,-./:<=>?@\\^`|~" :: String))

name :: Parser Text
name = label "name" (pack <$> some (satisfy isNameChar))

cord' :: Char -> Parser Text
cord' quo = pack <$> (char quo *> many(satisfy(/= quo)) <* char quo)

cord :: Parser (CordType, Text)
cord = ((THICK,) <$> cord' '"')
   <|> ((THIN,)  <$> cord' '\'')
   <|> ((CURL,)  <$> curl)

-- Parenthesis body in infix mode.  Occurs after initial form.
plix :: Form -> Parser Nest
plix initialForm = do
    fart (Right initialForm) >>= \case
        []        -> error "impossible"
        Left{}:_  -> error "impossible"
        [Right i] -> pure (WRAPD i)
        is ->
          case unravel [] is of
              ([f], [])             -> pure (WRAPD f)
              (fs,  [])             -> pure (PAREN fs)
              (fs,  (run,gs):more) -> do
                  assertUnambiguous "Nest" (run :| fmap fst more)
                  pure $ INFIX run (fs : gs : fmap snd more)
  where

    mixy = do { r <- try (rune <* whyt) ; (Left r :) <$> faro }

    term = char ')' $> []

    faro = do
        i <- Right <$> form
        fart i

    fart i = do
        is <- term <|> mixy <|> (whyt >> (term <|> mixy <|> faro))
        pure (i:is)

    ravel :: Text -> [Form] -> [Either Text Form] -> [(Text, [Form])]
    ravel r ws = \case
        []           -> (r, reverse ws) : []
        Left s  : is -> (r, reverse ws) : ravel s [] is
        Right w : is -> ravel r (w:ws) is

    unravel :: [Form] -> [Either Text Form] -> ([Form], [(Text, [Form])])
    unravel ws = \case
        []           -> (reverse ws, [])
        Left  r : is -> (reverse ws, ravel r [] is)
        Right w : is -> unravel (w:ws) is

-- Body of parenthesis in prefix mode, after initial (rune+space)
pree :: Text -> Parser Nest
pree initialRune = do
    fs <- end <|> frags
    pure (unravel initialRune [] fs)
  where
    end = [] <$ char ')'

    frags = (:) <$> frog <*> (end <|> do{whyt; end <|> frags})

    unravel roon ns = \case
        []           -> PREFX roon $ reverse ns
        Right w : is -> unravel roon (w:ns) is
        Left r : is  -> let deep = nestForm(unravel r [] is)
                        in PREFX roon (reverse(deep:ns))

    -- This is the same as `frag` but does not accept pages.
    frog :: Parser (Either Text Form)
    frog = (rinse <$> rune <*> optional form)
       <|> (Right <$> form)

    rinse :: Text -> Maybe Form -> Either Text Form
    rinse r = maybe (Left r) (Right . BEFO r)

para :: Parser Nest
para = do
    char '('
    spc0
    asum [ char ')' $> PREFX "|" []
         , try (rune <* char ')') >>= (pure . flip PREFX [])
         , try (rune <* whyt) >>= pree
         , form >>= plix
         ]

rune :: Parser Text
rune = pack <$> some runic

runic :: Parser Char
runic = label "runic" (satisfy isRuneChar)

nest :: Parser Nest
nest = brak <|> para

leaf :: Parser Leaf
leaf = (uncurry C <$> cord)
   <|> (N <$> name)

nestForm :: Nest -> Form
nestForm n = SHIP (NEST n :| [])

curl :: Parser Text
curl = char '{' >> loop 0 []
  where
    loop :: Int -> [Char] -> Parser Text
    loop !depth !acc = asum
        [ do char '\\'
             c <- printChar
             loop depth (c:acc)
        , do void (char '{')
             loop (depth+1) ('{' : acc)
        , do void (char '}')
             if depth==0
             then pure $ pack (reverse acc)
             else loop (depth-1) ('}' : acc)
        , do c <- printChar
             loop depth (c:acc)
        ]

brak :: Parser Nest
brak = do
    char '['
    spc0
    items <- (empt <|> carl)
    pure (PREFX "," items)
  where
    empt = [] <$ char ']'
    carl = (:) <$> form <*> (empt <|> (do whyt; empt <|> carl))

itmz :: Parser Itmz
itmz = (:|) <$> item <*> many item

item :: Parser Item
item = (NEST <$> nest) <|> (LEAF <$> leaf)

assertUnambiguous :: Text -> (NonEmpty Text) -> Parser ()
assertUnambiguous whichForm (r :| rs) = do
    let conflicts = filter (/= r) rs
    let notAllow = ".\nThis implementation doesn't allow ambiguous infix forms"
    case conflicts of
        _:_ ->
            fail
                $ unpack
                $ (<> notAllow)
                $ ((whichForm <> "-form mixes runes: ") <> )
                $ (intercalate ", ")
                $ map (\x -> "'" <> x <> "'")
                $ nub (r:conflicts)
        [] ->
            pure ()

shin :: Parser Form
shin = do
    i  <- itmz
    is <- many (try (ppair rune itmz))
    case is of
        []       -> pure (SHIP i)
        (r,j):ps -> do
            assertUnambiguous "Shut" (r :| fmap fst ps)
            pure (SHIN r (i : j : fmap snd ps))
  where
    ppair x y = (,) <$> x <*> y

form :: Parser Form
form = (BEFO <$> rune <*> shin) <|> shin

frag :: Parser (Int, Frag)
frag = (,) <$> getOffset
           <*> (  (uncurry PAGE <$> page)
              <|> (rinse <$> rune <*> optional form)
              <|> (FORM <$> form)
               )
  where
    rinse :: Text -> Maybe Form -> Frag
    rinse r = maybe (RUNE r) (FORM . BEFO r)

line :: Parser [(Int, Frag)]
line = spc0 >> (nada <|> pcons frag loan)
  where
    loan = nada <|> (whyt >> (nada <|> pcons frag loan))
    nada = ((char ';' >> takeRest) $> [])
       <|> (eof $> [])
    pcons x y = (:) <$> x <*> y

lexLine :: (FilePath, Int) -> Text -> Either Text [(Int, Frag)]
lexLine (pax, lin) txt = do
    case snd (runIdentity $ runParserT' line $ initialState pax lin txt)
      of Left  e -> Left (pack $ errorBundlePretty e)
         Right x -> Right x

initialState :: FilePath -> Int -> Text -> State Text e
initialState pax lin txt =
    State
    { stateInput = txt
    , stateOffset = 0
    , stateParseErrors = []
    , statePosState =
        PosState
        { pstateInput = txt
        , pstateOffset = 0
        , pstateTabWidth = mkPos 8
        , pstateLinePrefix = ""
        , pstateSourcePos = SourcePos pax (mkPos 1) (mkPos(lin+1))
        }
    }
