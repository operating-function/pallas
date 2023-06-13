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

import qualified Data.Vector as V

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
                           ReceiptRecv{..} -> toNoun (NAT 2, sender, reqIdx)
                           ReceiptSpun{..} -> toNoun (NAT 3, cogNum)
                           ReceiptStop{..} -> toNoun (NAT 4, cogNum)

instance FromNoun ReceiptItem where
    fromNoun n = case n of
        NAT 0                     -> Just ReceiptEvalOK
        ROW v -> case toList v of
            [NAT 1, val]          -> Just $ ReceiptVal val
            [NAT 2, cogN, reqN]   -> do
                sender <- fromNoun cogN
                reqIdx <- fromNoun reqN
                Just ReceiptRecv{..}
            [NAT 3, cogN]         -> do
                cogNum <- fromNoun cogN
                Just ReceiptSpun{..}
            [NAT 4, cogN]         -> do
                cogNum <- fromNoun cogN
                Just ReceiptStop{..}
            _                     -> Nothing
        _                         -> Nothing

instance ToNoun ResultReceipt where
    toNoun RESULT_OK           = NAT 0
    toNoun RESULT_CRASHED{..}  = ROW $ V.fromList [NAT 1, NAT op, arg]
    toNoun RESULT_TIME_OUT{..} = ROW $ V.fromList [NAT 2, toNoun timeoutAmount]

instance FromNoun ResultReceipt where
    fromNoun = \case
        NAT 0 -> pure $ RESULT_OK
        ROW v -> case toList v of
            [NAT 1, opFan, arg] -> do
                op <- fromNoun opFan
                Just RESULT_CRASHED{..}
            [NAT 2, timeoutFan] -> do
                timeoutAmount <- fromNoun timeoutFan
                Just RESULT_TIME_OUT{..}
            _     -> Nothing
        _ -> Nothing

instance ToNoun Receipt where
    toNoun RECEIPT{..} = toNoun (cogNum, result, inputs)

instance FromNoun Receipt where
    fromNoun n = do
        (cogNum,result,inputs) <- fromNoun n
        pure RECEIPT{..}

instance ToNoun LogBatch where
    toNoun LogBatch{..} =
        toNoun (batchNum, writeTime, executed)

instance FromNoun LogBatch where
    fromNoun n = do
        (batchNum,writeTime,executed) <- fromNoun n
        pure LogBatch{..}

instance ToNoun CogState where
  toNoun (CG_SPINNING fan)          = toNoun (NAT 0, fan)
  toNoun CG_CRASHED{op,arg,final}   = toNoun (NAT 1, NAT op, arg, final)
  toNoun CG_TIMEOUT{duration,final} = toNoun (NAT 2, duration, final)

instance FromNoun CogState where
  fromNoun n = case n of
    ROW v -> case toList v of
      [NAT 0, fan]                 -> Just $ CG_SPINNING fan
      [NAT 1, NAT op, arg, final]  -> Just $ CG_CRASHED{..}
      [NAT 2, natDuration, final] -> do
        duration <- fromNoun natDuration
        Just $ CG_TIMEOUT{..}
      _                            -> Nothing
    _ -> Nothing
