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

instance ToNoun CogId where toNoun (COG_ID i) = NAT $ fromIntegral i
instance FromNoun CogId where
    fromNoun n = (COG_ID . fromIntegral) <$> fromNoun @Nat n

instance ToNoun RequestIdx where
    toNoun (RequestIdx i) = NAT $ fromIntegral i
instance FromNoun RequestIdx where
    fromNoun n = (RequestIdx . fromIntegral) <$> fromNoun @Nat n

instance ToNoun TellId where
    toNoun (TellId i) = NAT $ fromIntegral i
instance FromNoun TellId where
    fromNoun n = (TellId . fromIntegral) <$> fromNoun @Nat n

instance ToNoun BatchNum where
    toNoun (BatchNum n) = toNoun n
instance FromNoun BatchNum where
    fromNoun n = BatchNum <$> fromNoun n

instance ToNoun ReceiptItem where
    toNoun re = toNoun $ case re of
        ReceiptEvalOK   -> NAT 0
        ReceiptVal val  -> toNoun (NAT 1, val)
        ReceiptTell{..} -> toNoun (NAT 2, asker, reqIdx, tellId)
        ReceiptAsk{..}  -> toNoun (NAT 3, tellId)
        ReceiptSpun{..} -> toNoun (NAT 4, cogNum)
        ReceiptReap{..} -> toNoun (NAT 5, cogNum)
        ReceiptStop{..} -> toNoun (NAT 6, cogNum)

instance FromNoun ReceiptItem where
    fromNoun n = case n of
        NAT 0                     -> Just ReceiptEvalOK
        ROW v -> case toList v of
            [NAT 1, val]          -> Just $ ReceiptVal val
            [NAT 2, askerN, reqN, tellN] -> do
                asker <- fromNoun askerN
                reqIdx <- fromNoun reqN
                tellId <- fromNoun tellN
                Just ReceiptTell{..}
            [NAT 3, tellN] -> do
                tellId <- fromNoun tellN
                Just ReceiptAsk{..}
            [NAT 4, cogN]         -> do
                cogNum <- fromNoun cogN
                Just ReceiptSpun{..}
            [NAT 5, cogN]         -> do
                cogNum <- fromNoun cogN
                Just ReceiptReap{..}
            [NAT 6, cogN]         -> do
                cogNum <- fromNoun cogN
                Just ReceiptStop{..}
            _                     -> Nothing
        _                         -> Nothing

instance ToNoun Receipt where
    toNoun RECEIPT_OK{..}       = toNoun (NAT 0, cogNum, inputs)
    toNoun RECEIPT_CRASHED{..}  = toNoun (NAT 1, cogNum, op, arg)
    toNoun RECEIPT_TIME_OUT{..} = toNoun (NAT 2, cogNum, timeoutAmount)

instance FromNoun Receipt where
    fromNoun = \case
        ROW v -> case toList v of
            [NAT 0, cogNumNoun, inputNoun] -> do
                cogNum <- fromNoun cogNumNoun
                inputs <- fromNoun inputNoun
                Just RECEIPT_OK{..}
            [NAT 1, cogNumNoun, NAT op, argFan] -> do
                cogNum <- fromNoun cogNumNoun
                arg <- fromNoun argFan
                Just RECEIPT_CRASHED{..}
            [NAT 2, cogNumNoun, timeoutFan] -> do
                cogNum <- fromNoun cogNumNoun
                timeoutAmount <- fromNoun timeoutFan
                Just RECEIPT_TIME_OUT{..}
            _     -> Nothing
        _ -> Nothing

instance ToNoun LogBatch where
    toNoun LogBatch{..} =
        toNoun (batchNum, writeTime, executed)

instance FromNoun LogBatch where
    fromNoun n = do
        (batchNum,writeTime,executed) <- fromNoun n
        pure LogBatch{..}

instance ToNoun CogState where
  toNoun (CG_SPINNING fan)          = toNoun (NAT 0, fan)
  toNoun (CG_FINISHED fan)          = toNoun (NAT 1, fan)
  toNoun CG_CRASHED{op,arg,final}   = toNoun (NAT 2, NAT op, arg, final)
  toNoun CG_TIMEOUT{duration,final} = toNoun (NAT 3, duration, final)

instance FromNoun CogState where
  fromNoun n = case n of
    ROW v -> case toList v of
      [NAT 0, fan]                 -> Just $ CG_SPINNING fan
      [NAT 1, fan]                 -> Just $ CG_FINISHED fan
      [NAT 2, NAT op, arg, final]  -> Just $ CG_CRASHED{..}
      [NAT 3, natDuration, final] -> do
        duration <- fromNoun natDuration
        Just $ CG_TIMEOUT{..}
      _                            -> Nothing
    _ -> Nothing
