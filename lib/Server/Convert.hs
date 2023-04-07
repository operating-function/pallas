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

instance ToNoun   CogName where toNoun   = toNoun . (.txt)
instance FromNoun CogName where fromNoun = fmap COG_NAME . fromNoun

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
                           ReceiptEvalOK  -> Nothing
                           ReceiptVal val -> Just val

instance FromNoun ReceiptItem where
    fromNoun n = maybe ReceiptEvalOK ReceiptVal <$> fromNoun n

instance ToNoun Receipt where
    toNoun re | re.didCrash = 0 %% (toNoun re{didCrash=False})
    toNoun re               = toNoun re.inputs

instance FromNoun Receipt where
    fromNoun = \case
        KLO _ xs -> case toList xs of
                        [NAT 0, TAB ts] -> RECEIPT True <$> decodeTab ts
                        _               -> Nothing
        TAB ts   -> RECEIPT False <$> decodeTab ts
        _        -> Nothing
      where
        decodeTab :: Map Fan Fan -> Maybe (IntMap ReceiptItem)
        decodeTab = fromNoun . TAB

instance ToNoun LogBatch where
    toNoun LogBatch{..} =
        toNoun (batchNum, writeTime, executed)

instance FromNoun LogBatch where
    fromNoun n = do
        (batchNum,writeTime,executed) <- fromNoun n
        pure LogBatch{..}
