-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Quick, crappy conversion between nouns and server data structures.
module Server.Convert where

import PlunderPrelude

import Fan
import Fan.Convert
import Server.Time
import Server.Types.Logging

--------------------------------------------------------------------------------

instance ToNoun NanoTime where
    toNoun (NanoTime n) = NAT n
instance FromNoun NanoTime where
    fromNoun (NAT n) = Just $ NanoTime n
    fromNoun _       = Nothing

instance ToNoun   MachineName where toNoun   = toNoun . (.txt)
instance FromNoun MachineName where fromNoun = fmap MACHINE_NAME . fromNoun

instance ToNoun CogId where toNoun (COG_ID i) = NAT $ fromIntegral i
instance FromNoun CogId where
    fromNoun n = (COG_ID . fromIntegral) <$> fromNoun @Nat n

instance ToNoun RequestIdx where
    toNoun (RequestIdx i) = NAT $ fromIntegral i
instance FromNoun RequestIdx where
    fromNoun n = (RequestIdx . fromIntegral) <$> fromNoun @Nat n

instance ToNoun BatchNum where
    toNoun (BatchNum n) = toNoun n
instance FromNoun BatchNum where
    fromNoun n = BatchNum <$> fromNoun n

instance ToNoun ReceiptItem where
    toNoun re = toNoun $ case re of
                           ReceiptEvalOK   -> NAT 0
                           ReceiptVal val  -> toNoun (NAT 1, val)
                           ReceiptRecv{..} -> toNoun (NAT 2, cogNum, reqNum)
                           ReceiptSpun{..} -> toNoun (NAT 3, cogNum)

instance FromNoun ReceiptItem where
    fromNoun n = case n of
        NAT 0                     -> Just ReceiptEvalOK
        ROW v -> case toList v of
            [NAT 1, val]          -> Just $ ReceiptVal val
            [NAT 2, cogN, reqN]   -> do
                cogNum <- fromNoun cogN
                reqNum <- fromNoun reqN
                Just ReceiptRecv{..}
            [NAT 3, cogN]         -> do
                cogNum <- fromNoun cogN
                Just ReceiptSpun{..}
            _                     -> Nothing
        _                         -> Nothing

instance ToNoun Receipt where
    toNoun RECEIPT{..} = toNoun (cogNum, didCrash, inputs)

instance FromNoun Receipt where
    fromNoun n = do
        (cogNum,didCrash,inputs) <- fromNoun n
        pure RECEIPT{..}

instance ToNoun LogBatch where
    toNoun LogBatch{..} =
        toNoun (batchNum, writeTime, executed)

instance FromNoun LogBatch where
    fromNoun n = do
        (batchNum,writeTime,executed) <- fromNoun n
        pure LogBatch{..}
