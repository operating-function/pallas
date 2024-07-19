-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall   #-}
{-# OPTIONS_GHC -Werror #-}

module Loot.Syntax
    ( resultEitherText
    , keyRex
    , readSigy
    , readArgs
    , readXTag
    , simpleTag
--  , tagText
    , tagNam
--  , tagStr
--  , tagRex
    , lawNameText
    , readCmd
    , rexCmd
    , valRex
    , isNameChar
    , readTag
    , readIdnt
    , readIdnTxt
    , readSymb
    , readBymb
--  , idnTag
    , symbRex
    , nameRex
    , textRex
    , parens
    , xtagApp
    , bodRex
    , joinRex
    , readKey
    , readName
    , readNumb
    , readNat
    , readCord
    , readCow
    , rForm1
    , rForm1c
    , rForm1N
    , rFormN
    , rFormN1c
    , rForm3c
    , rForm3
    , rForm2
    , rForm2c
    , rFormNc
    , rForm1Nc
    , Box(..)
    , bodBox
    , barBox
    , readOpenSet
    , readLineStr
    , boxRex
    , keyBox
    )
where

import Loot.Types
import PlunderPrelude
import Rex

import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.Text.Encoding      (decodeUtf8')
import Fan                     (lawNameText)
import Loot.Util               (lootRexToRex)

import qualified Data.ByteString.Base16 as B16
import qualified Data.Char              as C
import qualified Data.Text              as T


-- Types -----------------------------------------------------------------------

type Red v = ReadT v IO

data Box = BOX
    { boxWide  :: Maybe (Int, Rex)
    , boxTall  :: GRex Box
    }


-- Pretty Printing -------------------------------------------------------------

leafBox :: Int -> GRex Void -> Box
leafBox wid rex = BOX (Just (wid, rex)) (absurd <$> rex)

wideTextBox :: TextShape -> Text -> Box
wideTextBox s t =
    leafBox wid (T s t Nothing)
  where
    wid = length t + (if s==WORD then 0 else 2)

nameBox :: Text -> Box
nameBox = wideTextBox WORD

symbBox :: Symb -> Box
symbBox symb =
    case (symb, natUtf8 symb) of
        (0, _)                   -> haxHax (nameBox "0")
        (_, Right nm) | okIdn nm -> nameBox nm
        (_, Right nm) | okTxt nm -> haxHax (cordBox nm)
        _                        -> haxHax (nameBox (tshow symb))
  where
    haxHax :: Box -> Box
    haxHax b = BOX wid b.boxTall
      where
        wid = do
            (width, rex) <- b.boxWide
            pure $ ((1+width), N PREF "." [rex] NONE)


textBox :: TextShape -> Text -> Box
textBox style t =
    case (style, wideTextBox style t) of
      (LINE, bax) -> bax { boxTall = T LINE t Nothing }
      (_,    bax) -> bax

lineBox :: [Text] -> Box
lineBox linez =
    BOX Nothing (absurd <$> go linez)
  where
    go []     = T LINE "" Nothing
    go [l]    = T LINE l  Nothing
    go (l:ls) = T LINE l  (Just $ go ls)

cordBox :: Text -> Box
cordBox txt =
    case lines txt of
        []           -> textBox TEXT ""
        [l] | hasBot -> error "TODO: Escaping" l
        [l] | hasTic -> textBox TEXT l
        [l] | hasQuo -> textBox TEXT l -- TODO
        [l]          -> textBox TEXT l
        ls           -> lineBox ls
  where
    hasBot = hasTic && hasQuo
    hasTic = elem '\'' txt
    hasQuo = elem '"'  txt

natBox :: Nat -> Box
natBox n =
  case natUtf8 n of
    Left _                    -> decimal
    Right s | n==0            -> decimal
            | nonAscii s      -> decimal
            | all C.isAlpha s -> cenBox s
            | n<256           -> decimal
            | okCord s        -> textBox TEXT s -- TODO
            | okTape s        -> textBox TEXT s -- TODO
            | okCurl s        -> textBox TEXT s -- TODO
            | isLineStr s     -> lineBox [s]
            | isBlocStr s     -> lineBox (T.splitOn "\n" s)
            | otherwise       -> decimal
  where
    decimal = nameBox (tshow n)
    nonAscii s = any (not . C.isAscii) s

    isOkPrint '\n' = False
    isOkPrint '\t' = False
    isOkPrint c    = C.isPrint c

    isLineStr = all isOkPrint . unpack

    isBlocStr s = and [ not (T.null s)
                      , all isLineStr (T.splitOn "\n" s)
                      ]


-- XVAPP f x -> parens (valRexWid <$> unCell [x] f)
-- XVAPP f x -> niceApp (valRex <$> unXVApp [x] f)

forceBoxOpen :: Box -> Box
forceBoxOpen =
    \bax -> bax { boxWide = fmap go <$> bax.boxWide }
  where
    go :: Rex -> Rex
    go (C joke)         = absurd joke
    go x@(N OPEN _ _ _) = x
    go (N _ "|" p h)    = N OPEN "|" p h
    go x                = N OPEN "|" [x] NONE

valBox :: XVal -> Box
valBox = \case
    XVREF r   -> symbBox r
    XVNAT a   -> natBox a
    XVAPP f x -> boxApply (valBox <$> unCell [x] f)
    XVLAW l   -> lawBox l
    XVBAR b   -> barBox b
    XVROW r   -> rowBox r
    XVCOW n   -> nameBox ("C" <> tshow n)
    XVTAB t   -> tabBox t
    XVSET k   -> setBox k
    XVROX k   -> rexBox (lootRexToRex k)

valBoxes :: XVal -> [Box]
valBoxes = \case
    XVAPP f x -> valBox <$> unCell [x] f
    v         -> [valBox v]

rowBox :: Vector XVal -> Box
rowBox row =
    BOX wid tal
  where
    tal = if null row then
              (N NEST "," [] Nothing)
          else
              (openSeq "++" (fmap C . valBoxes <$> toList row))

    wid :: Maybe (Int, Rex)
    wid = do
        kids <- traverse ((.boxWide) . valBox) (toList row)
        pure ( 1 + length row + sum (fst <$> kids)
             , N NEST "," (snd <$> kids) Nothing
             )

rexBox :: GRex XVal -> Box
rexBox rex =
    BOX (Just (siz, wid)) tal
  where
    tal = N OPEN "`" [valBox <$> rex] Nothing
    wid = N PREF "`" [raw] Nothing
    raw = joinRex $ joinRex $ fmap valRex rex
    siz = length (let ?rexColors = NoColors in rexLine wid)

tabBox :: [(XVal, XVal)] -> Box
tabBox tab =
    BOX wid tal
  where
    wid :: Maybe (Int, Rex)
    wid = do
        wsk <- sequence wideSizedKeys

        let keySizes = fst <$> wsk
            wideKeys = snd <$> wsk

        let siz = if null tab
                  then 2
                  else 1 + length keySizes + sum keySizes

        pure ( siz
             , if null wideKeys then
                   N PREF "#" [N NEST "," [] NONE] NONE
               else
                   N NEST "," wideKeys NONE
             )

    tal :: GRex Box
    tal = openSeq "##"
        $ turn tab \(k,v) ->
            let kr = N PREF "=" [C(keyBox2 k)] Nothing
            in if keyIsValue k v
               then [kr]
               else kr : (C <$> valBoxes v)

    keyIsValue (XVNAT k) (XVREF w) = (k == w)
    keyIsValue _         _         = False


    wideSizedKeys :: [Maybe (Int, GRex Void)]
    wideSizedKeys = do
        (k, v) <- tab
        let kb = keyBox2 k
        let vb = valBox v
        pure $ case k of
            XVNAT kv | XVREF kv == v -> do
                    (kbWidth, kbWide) <- kb.boxWide
                    pure ( kbWidth + 1
                         , N PREF "=" [kbWide] NONE
                         )
            _ -> do
                    (kWidth, kRex) <- kb.boxWide
                    (vWidth, vRex) <- vb.boxWide
                    let wd = 1 + kWidth + vWidth
                    pure (wd, N SHUT "=" [kRex, vRex] NONE)


setBox :: [XVal] -> Box
setBox k =
    BOX wid tal
  where
    tal = openSeq "%%" (singleton . C <$> keysBox)
    keysBox = keyBox2 <$> toList k

    wid :: Maybe (Int, Rex)
    wid = do
        keyz <- traverse (.boxWide) keysBox
        let siz = 2 + sum (fst <$> keyz)
        let bod = N NEST "," (snd <$> keyz) NONE
        let rex = N PREF "%" [bod] NONE
        pure (siz, rex)

okCordChr, okTapeChr, okCurlChr :: Char -> Bool

okCordChr '\'' = False
okCordChr ' '  = True
okCordChr c    = C.isPrint c && not (C.isSpace c)

okTapeChr '"' = False
okTapeChr ' ' = True
okTapeChr c   = C.isPrint c && not (C.isSpace c)

okCurlChr ' ' = True
okCurlChr c   = C.isPrint c && not (C.isSpace c)

okCord, okTape, okCurl :: Text -> Bool

okCord t = length t < 40 && all okCordChr t
okTape t = length t < 40 && all okTapeChr t
okCurl t = length t < 40 && all okCurlChr t && curlNestOk 0 (unpack t)
  where
    curlNestOk :: Int -> String -> Bool
    curlNestOk n []       = (n == 0)
    curlNestOk n ('{':cs) = curlNestOk (n+1) cs
    curlNestOk 0 ('}':_ ) = False
    curlNestOk n ('}':cs) = curlNestOk (n-1) cs
    curlNestOk n (_  :cs) = curlNestOk n cs

okName :: Text -> Bool
okName txt = not (null txt) && all isNameChar txt && length txt < 40

barBox :: ByteString -> Box
barBox bs =
    fromMaybe fallbackResult (wideResult <|> linesResult)
  where
    hexTxt = toStrict $ decodeUtf8 $ toLazyByteString $ byteStringHex bs

    bsTxt = decodeUtf8' bs

    word c = T WORD c NONE

    wideResult =
        case bsTxt of
            Right t | okName t -> Just $
                BOX (Just (siz, rex)) (absurd <$> rex)
              where
                rex = N SHUT "#" [word "b", word t] NONE
                siz = 2 + length t

            -- TODO: Support more strings types (only newline + string
            -- uses all printable chars + string has non-printable chars)
            -- cannot be handled
            Right t | okCurl t -> Just $
                BOX (Just (siz, rex)) (absurd <$> rex)
              where
                rex = N SHUT "#" [word "b", T TEXT t NONE] NONE
                siz = 4 + length t

            _ -> Nothing

    fallbackResult :: Box
    fallbackResult =
        BOX (Just (siz, rex)) (absurd <$> rex)
      where
        siz = 2 + length hexTxt
        rex = N SHUT "#" [word "x", word hexTxt] NONE

    linesResult :: Maybe Box
    linesResult = do
        txt   <- either (const Nothing) Just bsTxt
        l:|ls <- pageLines txt
        let bx  = lineBox (l:ls)
        let tal = N OPEN "#" [T WORD "b" NONE] (Just bx.boxTall)
        pure (BOX Nothing tal)

pageLines :: Text -> Maybe (NonEmpty Text)
pageLines = \case
    tx | not (all ok tx) -> Nothing
    tx | otherwise       -> case (T.splitOn "\n" tx) of
                              []   -> Nothing
                              l:ls -> Just (l :| ls)
  where
    ok '\n' = True
    ok c    = C.isPrint c


boxRex :: Box -> Rex
boxRex box =
    case box.boxWide of
        Just (siz, rex) | siz<55 -> rex
        _                        -> joinRex (boxRex <$> box.boxTall)

valRex :: XVal -> GRex Rex
valRex = fmap absurd . boxRex . valBox

bodRex :: XBod -> GRex Rex
bodRex = fmap absurd . boxRex . bodBox

-- Printing --------------------------------------------------------------------

okIdn :: Text -> Bool
okIdn txt =
    fromMaybe False $ do
        guard (not $ null txt)
        (c, _) <- T.uncons txt
        guard (not $ C.isDigit c)
        pure (all okIdnChar txt)
  where
    okIdnChar '_' = True
    okIdnChar c   = C.isAlphaNum c

textRex :: TextShape -> Text -> GRex v
textRex s t = T s t Nothing

nameRex :: Text -> GRex v
nameRex = textRex WORD

cenBox :: Text -> Box
cenBox text = leafBox (length text + 1) rex
  where
    rex = N PREF "%" [T WORD text Nothing] Nothing

simpleBody :: XBod -> Bool
simpleBody XVAR{}         = True
simpleBody (XRAW XVNAT{}) = True
simpleBody (XRAW XVCOW{}) = True
simpleBody _              = False

-- TODO Explicitly check if recursive somehow.
--         If we have a self-reference, we need to use the `?` form.
--         So, actually that check should happen in `resugar`
--         And the XLAW type should explicitly say (no name vs name).
lawBox :: XLaw -> Box
lawBox = \case
    XLAM r b ->
         let rb = argsBox r
         in boxNode "&" (mode, [rb, bb], NONE)
                        ([rb], Just bb)
      where
        bb = bodBox b
        mode = if simpleBody b && length r < 2
               then SHUT
               else INFX
    XLAW t r b ->
        let sb = sigBox t r
        in boxNode "?" (INFX, [sb,bb], NONE)
                       ([sb], Just bb)
      where bb = bodBox b

boxNode :: Text
    -> (RuneShape, [Box], Maybe Box)
    -> ([Box], Maybe Box)
    -> Box
boxNode roon (wStyle, wSons, wHeir) (tSons, tHeir) =
    BOX wid tal
  where
    tal =
        N OPEN roon (C <$> tSons) (C <$> tHeir)

    wid = do
        sons <- traverse (.boxWide) wSons
        heir <- traverse (.boxWide) wHeir
        Just (mkWide wStyle sons heir)


    mkWide :: RuneShape -> [(Int,Rex)] -> Maybe(Int,Rex) -> (Int, Rex)
    mkWide style sons heir =
        case style of
            OPEN  ->
                mkWide NEST sons heir

            NEST ->
                ( ( 1
                  + length roon
                  + length sons
                  + sum (fst <$> sons)
                  + fromMaybe 0 (fst <$> heir)
                  )
                , N NEST roon (snd <$> sons) (snd <$> heir)
                )

            PREF ->
                case (sons, heir) of
                    ([(s,w)], Nothing) ->
                        ( 1 + s
                        , N PREF roon [w] NONE
                        )
                    _ ->
                        mkWide NEST sons heir


            SHUT ->
                case snd <$> (sons <> toList heir) of
                    []  -> mkWide NEST sons heir
                    [_] -> mkWide NEST sons heir
                    xs  -> ( length roon * (length xs - 1) + sum (fst <$> sons)
                           , N SHUT roon (snd <$> sons) (snd <$> heir)
                           )

            INFX ->
                -- TODO WAAAY overly simplistic
                let nk = length sons in
                if nk<2 then
                    mkWide NEST sons heir
                else
                    let siz =
                            ( 2
                            + ((nk-1) * (2+length roon))
                            + sum (fst <$> sons)
                            + maybe 0 fst heir
                            )
                    in
                        ( siz
                        , N INFX roon (snd <$> sons) (snd <$> heir)
                        )

keyBox :: Nat -> Box
keyBox 0   = nameBox "0"
keyBox nat =
    case natUtf8 nat of
        Right n | okIdn n -> nameBox n
        Right t | okTxt t -> cordBox t
        _                 -> natBox nat

keyBox2 :: XVal -> Box
keyBox2 (XVNAT n) = keyBox n
keyBox2 other     = valBox other

okTxt :: Text -> Bool
okTxt txt =
    all C.isPrint txt && not (hasTic && hasQuo)
  where
    hasTic = elem '\'' txt
    hasQuo = elem '"'  txt

unCell :: [XVal] -> XVal -> [XVal]
unCell acc = \case
    XVAPP f x -> unCell (x:acc) f
    x         -> (x:acc)

parens :: [GRex v] -> GRex v
parens rexz = N NEST "|" rexz Nothing

pattern NONE :: Maybe a
pattern NONE = Nothing

runeSeq :: Text -> [GRex v] -> GRex v
runeSeq _   []     = error "runeSeq: impossible"
runeSeq ryn [x]    = N OPEN ryn [x] Nothing
runeSeq ryn (x:xs) = N OPEN ryn [x] (Just $ runeSeq ryn xs)

openSeq :: Text -> [[GRex Box]] -> GRex Box
openSeq roon = go
  where
    go []     = N OPEN roon [] Nothing
    go [x]    = N OPEN roon x Nothing
    go (x:xs) = N OPEN roon x (Just $ go xs)

simpleAppBox :: [Box] -> Box
simpleAppBox kids =
    BOX wid tal
  where
    tal = N OPEN "|" (C <$> kids) Nothing

    wid = do
        wKids <- traverse (.boxWide) kids
        pure ( 1 + length kids + sum (fst <$> wKids)
             , N NEST "|" (snd <$> wKids) Nothing
             )

sigBox :: XTag -> (NonEmpty Symb) -> Box
sigBox n (r:|rs) = simpleAppBox (xtagBox n : fmap symbBox (r:rs))

argsBox :: (NonEmpty Symb) -> Box
argsBox (n:|[]) = symbBox n
argsBox (n:|ns) = simpleAppBox (symbBox <$> (n:ns))


{-
    TODO Right now `$a == a`
    TODO Make $a_3 be a shorthand for "a_3$a".
-}
xtagBox :: XTag -> Box
xtagBox = \case
    XTAG n Nothing  -> symbBox n
    XTAG n (Just t) ->
        BOX wid tal
      where
        tal =
            N OPEN "@" (C <$> [keyBox n, keyBox t]) NONE

        wid = do
            (nw, nr) <- (keyBox n).boxWide
            (tw, tr) <- (keyBox t).boxWide
            Just ( nw + 1 + tw
                 , N SHUT "@" [nr,tr] NONE
                 )

joinRex :: GRex (GRex v) -> GRex v
joinRex = \case
    T s t k   -> T s t (joinRex <$> k)
    C c       -> c
    N m r x k -> N m r (joinRex <$> x) (joinRex <$> k)

unXApp :: [XBod] -> XBod -> [XBod]
unXApp acc = \case
    XAPP f x -> unXApp (x:acc) f
    x        -> (x:acc)

bodBox :: XBod -> Box
bodBox = \case
    XVAR v           -> symbBox v
    XRAW x           -> valBox x
    XCNS (XVAPP f x) -> boxShutInfix "!" (valBox <$> unCell [x] f)
    XBAD (XVAPP f x) -> boxShutInfix ":" (valBox <$> unCell [x] f)
    XCNS x           -> boxPrefix "!" (valBox x)
    XBAD x           -> boxPrefix ":" (valBox x)
    XAPP f x         -> bodApply (unXApp [x] f)
    XLET n v k       -> letBox n v k

letBox :: Symb -> XBod -> XBod -> Box
letBox n v h =
    BOX wid tal
  where
    tal = N OPEN "@"
            (C <$> [symbBox n, bodBox v])
            (Just $ boxCont $ bodBox h)

    wid = do
        nb   <- (symbBox n).boxWide
        bb   <- (bodBox v).boxWide
        hb   <- (bodBox h).boxWide
        let kids = snd <$> [nb, bb]
        let wd   = 5 + sum (fst <$> [nb,bb,hb])
        pure (wd, N INFX "@" kids (Just $ snd hb))
        -- TODO Size is correct except if the value-expression is an
        -- application, which will be then be unpacked (subtract 2) Also,
        -- need to check if continuation will be wrapped, and add 2 if so.

boxCont :: Box -> GRex Box
boxCont = C . forceBoxOpen

bodApply :: [XBod] -> Box
bodApply exprs =
    fromMaybe (boxApply boxes) wid
  where
    boxes = bodBox <$> exprs

    wid = do
        sons <- traverse (.boxWide) boxes
        pure $
            if sum (fst <$> sons) < 40 then
                boxApply boxes
            else
                case span (\((w,_),_) -> w < 10) (reverse (zip sons boxes)) of
                    ([], _) -> boxApply boxes
                    (_, []) -> boxApply boxes
                    (r, b)  -> ketBox (reverse $ snd <$> r) (reverse $ snd <$> b)

{-
    TODO: Adopt ^ rune from Sire and adjust pretty-printer to take
    full advantage.
-}
ketBox :: [Box] -> [Box] -> Box
ketBox args body =
    foo { boxTall = tal }
  where
    foo = boxApply (body <> args)
    tal = N OPEN "^" (C <$> args) (Just $ C $ boxApply body)

boxApply :: [Box] -> Box
boxApply [] =
    BOX (Just (3, (N NEST "|" [] Nothing)))
      (N NEST "|" [] Nothing)
boxApply [bix] =
    bix
boxApply boxes =
    BOX wid tal
  where
    wid = do
        wbox <- traverse (.boxWide) boxes
        let rexz = (snd <$> wbox)
        let hepSz = (length wbox - 1) + sum (fst <$> wbox)
        let barSz = 1 + length boxes + sum (fst <$> wbox)
        case rexz of
            [f,_] | isPrim f ->
                 pure (hepSz, N SHUT  "-" rexz Nothing)

            _ | all simple rexz ->
                 pure (hepSz, N SHUT  "-" rexz Nothing)

            _ ->
                 pure (barSz, N NEST "|" rexz Nothing)

    isPrim (T WORD "0" Nothing) = True
    isPrim (T WORD "1" Nothing) = True
    isPrim (T WORD "2" Nothing) = True
    isPrim _                    = False

    simple (T WORD _ Nothing) = True
    simple _                  = False

    tal = case reverse boxes of
        []     -> N OPEN "|" [] Nothing
        [x]    -> N OPEN "|" [C x] Nothing
        k:l:sx -> N OPEN "|" (C <$> reverse (l:sx))
                             (Just $ C $ forceBoxOpen k)


boxPrefix :: Text -> Box -> Box
boxPrefix roon expr =
    BOX wid tal
  where
    tal = N OPEN roon [C expr] NONE

    -- TODO Size calculation is correct unless the rex printer needs to
    -- wrap the result in parenthesis.
    wid = do
        (xWid, xRex) <- expr.boxWide
        pure ( length roon + xWid
             , N PREF roon [xRex] NONE
             )

boxShutInfix :: Text -> [Box] -> Box
boxShutInfix roon exprs =
    BOX wid tal
  where
    tal = runeSeq roon (C <$> exprs)

    wid :: Maybe (Int, Rex)
    wid = do
        sons <- traverse (.boxWide) exprs

        pure $ case sons of
          [] ->
              ( 2 + length roon
              , N NEST roon [] NONE
              )

          [(w,r)] ->
              ( 3 + length roon + w
              , N NEST roon [r] NONE
              )

          _ ->
              ( (length roon * (length sons - 1)) + sum (fst <$> sons)
              , N SHUT roon (snd <$> sons) NONE
              )


-- TODO Hack for printing in ReplExe -------------------------------------------

xtagApp :: XTag -> NonEmpty Symb -> Rex
xtagApp t as = parens (xtagRex t : fmap symbRex (toList as))

xtagRex :: XTag -> Rex
xtagRex = \case
    XTAG n Nothing  -> keyRex n
    XTAG n (Just t) -> N SHUT "@" [keyRex n, keyRex t] NONE

boxCoerceWideRex :: Box -> Rex
boxCoerceWideRex (BOX (Just(_, wid)) _) = wid
boxCoerceWideRex box                    = boxRex box

keyRex :: Symb -> Rex
keyRex = boxCoerceWideRex . keyBox

symbRex :: Symb -> Rex
symbRex symb =
    case (symb, natUtf8 symb) of
        (0, _)                   -> haxHax (nameRex "0")
        (_, Right nm) | okIdn nm -> nameRex nm
        (_, Right nm) | okTxt nm -> haxHax (cordRex nm)
        _                        -> haxHax (nameRex (tshow symb))
  where
    haxHax n = N PREF "." [n] Nothing
    cordRex = boxCoerceWideRex . cordBox

-- Parsing ---------------------------------------------------------------------

rexCmd :: ∀a. Show a => Rex -> Either Text (XCmd a)
rexCmd rex = resultEitherText id rex (runReading readCmd rex)

runReading :: Red v a -> GRex v -> Result v a
runReading act = unsafePerformIO . runResultT . runReadT act

resultEitherText :: (GRex v -> Rex) -> Rex -> Result v a -> Either Text a
resultEitherText cvt blk res =
    let ?rexColors = NoColors in
    case resultEither res of
      Right x       -> pure x
      Left (mR,err) -> Left (errMsg <> maybe "" showForm mR <> showBlock)
        where
          errMsg      = unlines["== READER ERROR ==", "", err ]
          showForm rx = unlines["In Form:" , "" , dent "  " (rexFile $ cvt rx) ]
          showBlock   = unlines["In Block:" , "" , dent "  " (rexFile blk) ]

dent :: Text -> Text -> Text
dent pre = unlines . fmap dentLine . lines
  where
    dentLine :: Text -> Text
    dentLine "" = pre
    dentLine ln = pre <> "  " <> ln

readCmd :: Red v (XCmd a)
readCmd = asum
    [ do rune "??"
         checks <- slip1N "??" readVal readVal
         pure $ XASSERT (checks <&> \(v,vs) -> foldl' XVAPP v vs)
    , do rune "<"
         (v,vs) <- form1Nc readVal readVal
         pure (XDUMPIT $ foldl' XVAPP v vs)
    , do rune "="
         binds <- slip2 "=" readBind readBod
         XDEFINE <$> traverse (uncurry mkBind) binds
    , XOUTPUT <$> readVal
    ]

mkBind :: XBindHead -> XBod -> Red v XBind
mkBind hed bod = case hed of
    XHEAD_PL t xs -> pure $ XBIND_PL (XLAW t xs bod)
    XHEAD_VL t    -> XBIND_VL t <$> bodCastToVal bod
    XHEAD_PN t    -> XBIND_PN t <$> bodCastToVal bod

bodCastToVal :: XBod -> Red v XVal
bodCastToVal = go
  where
    go = \case
        XRAW v   -> pure v
        XCNS v   -> pure v
        XBAD v   -> pure v
        XVAR t   -> pure (XVREF t)
        XAPP f x -> XVAPP <$> go f <*> go x
        XLET{}   -> throwError "No lets in value-definitions"

readBind :: Red v XBindHead
readBind = asum
    [ XHEAD_VL <$> readBymb
    , do (rune "|" <|> rune "-")
         asum [ XHEAD_PN <$> form1 readBymb
              , form2N readXTag readSymb readSymb <&> \(t,x,xs) -> XHEAD_PL t (x:|xs)
              ]
    ]

cenLit :: Red v XVal
cenLit = do
    rune "%"
    form1 $ asum
        [ XVNAT . utf8Nat <$> readName
        , readWideTabish
        ]

readVal :: Red v XVal
readVal = asum
    [ cenLit
    , XVNAT <$> readNat
    , XVCOW <$> readCow
    , XVREF <$> readSymb
    , XVBAR <$> readBar
    , rForm2c "?" readSigy readBod lamb
    , rForm2c "&" readArgs readBod anon
    , rFormNc "," readVal (XVROW . fromList)
    , rune "##" >> readOpenTabish
    , do rune "++"
         sequ <- slip1N "++" readVal readVal
         pure $ XVROW $ fromList $ fmap (uncurry xvap) sequ
    , rFormN1c "." readVal readVal appTo
    , do (rune "|" <|> rune "-")
         (x, xs) <- form1Nc readVal readVal
         let cell a []    = a
             cell a (b:c) = cell (XVAPP a b) c
         pure (cell x xs)
    ]
  where
    appTo xs f = apple f xs

    apple f []    = f
    apple f (b:c) = apple (XVAPP f b) c

    lamb (x,a) b = XVLAW (XLAW x a b)
    anon rs c    = XVLAW (XLAM rs c)
    xvap = foldl' XVAPP

-- TODO Tabs now use `=key` an sets just use `key`.
readOpenTabish :: Red v XVal
readOpenTabish = do
    rex <- readRex
    case rex of
        N _ "##" (N _ "=" [_] _ : _) _ -> readOpenTab
        _                              -> XVSET <$> readOpenSet

readOpenTab :: Red v XVal
readOpenTab = do
    pairs <- slip1N "##" (rune "=" >> form1 readKey2) readVal
    let keySet = setFromList (fst <$> pairs) :: Set XVal
    --
    when (length pairs /= length keySet) do
        -- TODO Find and show the offending key.
        throwError "Duplicated Key"
    --
    pure $ XVTAB $ turn pairs \case
        ( XVREF k, []   ) -> (XVNAT k, XVREF k          )
        ( kExp,    []   ) -> (kExp,    kExp             )
        ( k,       x:xs ) -> (k,       foldl' XVAPP x xs)

readKey2 :: ReadT v IO XVal
readKey2 = readVal -- TODO

readOpenSet :: Red v [XVal]
readOpenSet = do
    keyList <- slip1 "%%" readKey2
    let keySet = setFromList keyList :: Set XVal
    when (length keyList /= length keySet) do
        -- TODO Find and show the offending key.
        throwError "Duplicated Key"
    pure keyList

readWideTabish :: Red v XVal
readWideTabish = either XVSET XVTAB <$> do
    rune ","

    let keyEle (XVREF k) = (XVNAT k, XVREF k)
        keyEle k         = (k, k)

    res :: [Either XVal (XVal,a)]
        <- formN $ asum
                 [ Left <$> readKey2
                 , Right <$> (rune "=" >> (    form2 readKey2 readVal
                                          <|> (keyEle <$> form1 readKey2)
                                          ))
                 ]

    let goRyt acc []                              = pure (Right acc)
        goRyt _   (Left _     :_)                 = sawBoth
        goRyt acc (Right (k,_):_)  | member k acc = dups k
        goRyt acc (Right (k,v):ks) | otherwise    = goRyt (insertMap k v acc) ks

    let goLef acc []                        = pure (Left acc)
        goLef _   (Right _:_)               = sawBoth
        goLef acc (Left k :_)  | elem k acc = dups k
        goLef acc (Left k :ks) | otherwise  = goLef (k : acc) ks

    case res of
      []               -> pure (Right mempty)
      Right (k,v) : es -> goRyt mempty        (Right (k,v) : es)
      Left  e     : es -> goLef (singleton e) es
  where
    sawBoth = throwError "Cannot mix %[a] and %[a=3] forms."
    dups k  = throwError ("Key appears twice in tab literal: " <> tshow k)

readKey :: Red v Nat
readKey = asum
    [ readNumb
    , readIdnt
    , utf8Nat <$> readCord
    ]

readArgs :: Red v (NonEmpty Symb)
readArgs = asum
    [ singleton <$> readSymb
    , do rune "|"
         (x,xs) <- form1N readSymb readSymb
         pure (x :| xs)
    ]

readSigy :: Red v (XTag, NonEmpty Symb)
readSigy = do
    rune "|"
    (t,x,xs) <- form2N readXTag readSymb readSymb
    pure (t, x:|xs)

rForm2c :: Text -> Red v a -> Red v b -> (a -> b -> c) -> Red v c
rForm2c r x y k = rune r >> form2c x y >>= \(a,b) -> pure (k a b)

rForm1c :: Text -> Red v a -> (a -> b) -> Red v b
rForm1c r x k = rune r >> form1c x >>= \a -> pure (k a)

rForm1 :: Text -> Red v a -> (a -> b) -> Red v b
rForm1 r x k = rune r >> form1 x >>= \a -> pure (k a)

rFormN :: Text -> Red v a -> ([a] -> b) -> Red v b
rFormN r x k = rune r >> formN x >>= \a -> pure (k a)

rForm1N :: Text -> Red v a -> Red v b -> (a -> [b] -> c) -> Red v c
rForm1N r x y k = rune r >> form1N x y >>= \(a,b) -> pure (k a b)

rFormNc :: Text -> Red v a -> ([a] -> b) -> Red v b
rFormNc r x k = rune r >> formNc x >>= \a -> pure (k a)

rFormN1c :: Text -> Red v a -> Red v b -> ([a] -> b -> c) -> Red v c
rFormN1c r x y k = rune r >> formN1c x y >>= \(a,b) -> pure (k a b)

rForm3c :: Text -> Red v a -> Red v b -> Red v c -> (a -> b -> c -> d)
        -> Red v d
rForm3c r x y z k = rune r >> form3c x y z >>= \(a,b,c) -> pure (k a b c)

rForm3 :: Text -> Red v a -> Red v b -> Red v c -> (a -> b -> c -> d)
       -> Red v d
rForm3 r x y z k = rune r >> form3 x y z >>= \(a,b,c) -> pure (k a b c)

rForm2 :: Text -> Red v a -> Red v b -> (a -> b -> c) -> Red v c
rForm2 r x y k = rune r >> form2 x y >>= \(a,b) -> pure (k a b)

rForm1Nc :: Text -> Red v a -> Red v b -> (a -> [b] -> c) -> Red v c
rForm1Nc r x y k = rune r >> form1Nc x y >>= \(a,bs) -> pure (k a bs)


readCow :: Red v Nat
readCow = matchLeaf "C[0-9]+" \case
    (WORD, "C0") -> Just 0

    (WORD, (unpack -> ('C' : n : ns))) -> do
        guard (all C.isDigit (n:ns))
        guard (n /= '0')
        readMay (n:ns)
    _ -> Nothing

readBod :: Red v XBod
readBod = asum
    [ XRAW <$> cenLit
    , XRAW . XVNAT <$> readNat
    , XRAW . XVBAR <$> readBar
    , readCow <&> \case 0 -> XRAW (XVROW mempty)
                        n -> XRAW (XVCOW n)
    , XVAR <$> readSymb
    , rFormN1c "." readBod readBod appTo
    , rune "|" >> (uncurry xapp <$> form1Nc readBod readBod)
    , rune "-" >> (uncurry xapp <$> form1Nc readBod readBod)
    , do rune ":"
         asum
             [ uncurry xbad <$> form1N readVal readVal
             , xbab <$> slip1 ":" readVal
             ]
    , rune "!" >>
          asum
              [ uncurry xcns <$> form1N readVal readVal
              , xkak <$> slip1 "!" readVal
              ]
    , rune "@" >> (xlet <$> form3c readSymb readBod readBod)
    , rForm2c "?" readSigy readBod lamb
    , rForm2c "&" readArgs readBod anon
    , rune "##" >> fmap XRAW readOpenTabish

    -- TODO Separate out vals that are read XRAW, to avoid this
    -- duplication.
    , rFormNc "," readVal (XRAW . XVROW . fromList)
    , do rune "++"
         sequ <- slip1N "++" readVal readVal
         pure $ XRAW $ XVROW $ fromList $ fmap (uncurry xvap) sequ
    ]
  where
    appTo xs f = apple f xs

    -- TODO Foldl?
    apple f []    = f
    apple f (b:c) = apple (XAPP f b) c

    xlet = \(n,v,k) -> XLET n v k
    xbad = \v vx -> XBAD (xvap v vx)
    xbab = \case []   -> error "readBod.xbab: impossible"
                 v:vx -> xbad v vx
    xkak = \case []   -> error "readBod.xkak: impossible"
                 v:vx -> xcns v vx
    xcns = \v vx -> XCNS (xvap v vx)
    xvap = foldl' XVAPP
    xapp = foldl' XAPP
    lamb (t,as) b = XRAW $ XVLAW (XLAW t as b)
    anon as     c = XRAW $ XVLAW (XLAM as c)

readTag :: Red v XTag
readTag = (simpleTag <$> readIdnt)
      <|> ((rune "." <|> rune "#.") >> haxTag)
  where
    haxTag = keyTag <|> explicitTag
    keyTag = simpleTag <$> readKey
    explicitTag = do
        (n,t) <- form2 readKey readKey
        pure (XTAG n (Just t))

readXTag :: Red v XTag
readXTag = asum
    [ idnXTag <$> readKey
    , rune "@" >> asum
          [ form2 readKey readKey <&> \(n,t) -> XTAG n (Just t)
          , form1 readKey         <&> \n     -> XTAG n Nothing
          ]
    ]
  where
    idnXTag n = XTAG n NONE

simpleTag :: Symb -> XTag
simpleTag t = XTAG t Nothing

readIdnt :: Red v Symb
readIdnt = utf8Nat <$> readIdnTxt

readIdnTxt :: Red v Text
readIdnTxt = matchLeaf "symbol" \case
    (WORD,n) | okIdn n -> Just n
    _                  -> Nothing

readSymb :: Red v Symb
readSymb = readIdnt <|> ((rune "." <|> rune "#.") >> form1 readKey)

readBymb :: Red v Symb
readBymb = readKey <|> ((rune "." <|> rune "#.") >> form1 readKey)

readName :: Red v Text
readName = matchLeaf "name" \case
    (WORD,n) -> Just n
    _        -> Nothing

readCord :: Red v Text
readCord = matchLeaf "cord" \case
    (TEXT,t) -> Just t
    _        -> Nothing

readLineStr :: Red v Text
readLineStr = matchLeaf "lineStr" \case
    (LINE,l) -> Just l
    _        -> Nothing

isNameChar :: Char -> Bool
isNameChar '_' = True
isNameChar c   = C.isAlphaNum c

readNat :: Red v Nat
readNat =
    readNumb <|> readColn <|> readTape <|> line
  where
    line = utf8Nat <$> readLineStr
    readTape = utf8Nat <$> readCord
    readColn = rune "%" >> form1 (utf8Nat <$> readName)

readNumb :: Red v Nat
readNumb =
    matchName "nat" \n ->
    case unpack n of
        '_':_ -> Nothing
        _     -> readMay (filter (/= '_') n)

readAnyText :: Red v Text
readAnyText = (readName <|> readCord <|> readLineStr)

-- TODO Use x#_ and b#_ syntax
readBar :: Red v ByteString
readBar = do
    rune "#"
    (n, k) <- form2 readName readAnyText
    case n of
        "b" -> pure (encodeUtf8 k)
        "x" -> case B16.decode (encodeUtf8 k) of
                   Left er -> throwError ("Invalid hex literal: " <> pack er)
                   Right x -> pure x
        _   -> throwError "Invalid # literal"
