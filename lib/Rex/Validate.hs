-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall -Werror #-}

{-
    This reports parsing errors for rex blocks.  These are the only
    possible errors:

    -   Unexpected characters

    -   Unterminated nested forms

    -   Unterminated string literals

    -   Infix/prefix runes in [ bracket * forms ]
-}

module Rex.Validate (checkTree) where

import PlunderPrelude hiding (many, head, last)
import Rex.Mechanism
import Control.Monad.Writer

import Data.List.NonEmpty (head)

import qualified Data.ByteString as BS


-- Policy ----------------------------------------------------------------------

err :: Span a -> Text -> Writer [Text] ()
err s reason = tell $ singleton $ concat $
    [ pack s.lin.fil, ":", tshow s.lin.num, ":", tshow (s.off + 1)
    , " error:"
    , "\n\n    ", reason
    , "\n\n\t", decodeUtf8 s.lin.byt
    , "\n\t", colPointer s
    ]

colPointer :: Span a -> Text
colPointer s =
    concat [ replicate s.off ' '
           , replicate (s.end - s.off) '^'
           ]

curlCheck :: Elem -> Writer [Text] ()
curlCheck e =
    if e.lin.byt BS.!? e.off == Just 34
    then unless (e.lin.byt BS.!? pred e.end == Just 34) (err e "unterminated")
    else go (e.off + 1) 1
  where
    go :: Int -> Int -> Writer [Text] ()
    go _ 0 = pure ()
    go i d = case e.lin.byt BS.!? i of
                 Nothing  -> err e "unterminated"
                 Just 123 -> go (i+1) (d+1)
                 Just 125 -> go (i+1) (d-1)
                 Just _   -> go (i+1) d

checkElem :: (?inCurl :: Bool) => Elem -> Writer [Text] ()
checkElem e =
    case e.x of
       PARA cs -> checkNest e cs ')'
       BRAK cs -> do
           checkNest e cs ']'
           for_ (snd . clumpFrag <$> cs) \case
               WOLF _ (C c) -> unless ?inCurl do err (head c.x) noRunes
               LAMB{}       -> pure ()

       CORD CURLY -> curlCheck e

       CORD QUOTED -> do
           let bs = spanBar e
           let wid = length bs
           unless (bs BS.!? 1 == bs BS.!? (wid-1)) do
               err e "unterminated string"

       FAIL -> do
           let c = toEnum $ fromEnum $ unsafeHead $ spanBar e
           err e ("Unexpected: " <> tshow (c::Char))

       LINE more -> do
           for_ ((const () <$> e) : more) \x -> do
               case spanBar x BS.!? 1 of
                   Nothing -> pure ()
                   Just 32 -> pure ()
                   _       -> err e "}-string must start with a space"

       _ -> do
           pure ()
  where
    noRunes = "Brackets may not contain runes"

checkNest :: (?inCurl :: Bool) => Elem -> [Clump] -> Char -> Writer [Text] ()
checkNest e cs endChar = do
    traverse_ checkClump cs
    let endCharCode = toEnum (fromEnum endChar) :: Word8
    let bs = spanBar e
    let wid = length bs
    if (Just endCharCode /= bs BS.!? (wid-1)) then
        err e "unterminated nesting"
    else
        case reverse cs of
            []      -> pure ()
            C x : _ -> when (x.end == e.end) do
                           err e "unterminated nesting"

checkClump :: (?inCurl :: Bool) => Clump -> Writer [Text] ()
checkClump (C cs) = traverse_ checkElem cs.x

checkTree :: Tree -> Writer [Text] ()
checkTree (LEAF c)           = let ?inCurl = False in checkClump c
checkTree (NODE _ sons heir) = traverse_ checkTree (sons <> toList heir)
