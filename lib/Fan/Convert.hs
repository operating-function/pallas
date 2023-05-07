-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Fan.Convert where

import Fan
import Numeric.Natural
import PlunderPrelude

import GHC.Word (Word(W#))

import qualified Data.Map    as M
import qualified Data.Vector as V
import qualified GHC.Natural as GHC

import Data.Vector ((!))


--------------------------------------------------------------------------------

class ToNoun a where
    toNoun :: a -> Fan

class FromNoun a where
    fromNoun :: Fan -> Maybe a

instance ToNoun () where
    toNoun () = NAT 0

instance FromNoun () where
    fromNoun (NAT 0) = Just ()
    fromNoun _       = Nothing

instance ToNoun Bool where
    toNoun True  = NAT 1
    toNoun False = NAT 0

instance FromNoun Bool where
    fromNoun (NAT 0) = Just False
    fromNoun (NAT 1) = Just True
    fromNoun _       = Nothing

instance ToNoun Fan where
    toNoun = id
instance FromNoun Fan where
    fromNoun = Just . id

instance ToNoun Pin where
    toNoun p = PIN p
instance FromNoun Pin where
    fromNoun (PIN p) = Just p
    fromNoun _       = Nothing

instance ToNoun Natural where
    toNoun n = NAT n
instance FromNoun Natural where
    fromNoun (NAT n) = Just n
    fromNoun _       = Nothing

instance ToNoun Word where
    toNoun (W# w) = NAT (GHC.NatS# w)
instance FromNoun Word where
    fromNoun (NAT (GHC.NatS# w)) = Just (W# w)
    fromNoun _                   = Nothing

instance ToNoun Word64 where
    toNoun w = toNoun (fromIntegral w :: Word)
instance FromNoun Word64 where
    fromNoun n = do
        w :: Word <- fromNoun n
        pure (fromIntegral w)

instance (ToNoun a,ToNoun b) => ToNoun (a,b)
  where toNoun(x,y) = mkRow[toNoun x, toNoun y]

instance (ToNoun a,ToNoun b,ToNoun c) => ToNoun (a,b,c)
  where toNoun(x,y,z) = mkRow[toNoun x, toNoun y, toNoun z]

instance (ToNoun a,ToNoun b,ToNoun c,ToNoun d) => ToNoun (a,b,c,d)
  where toNoun(p,q,r,s) = mkRow[toNoun p,toNoun q,toNoun r,toNoun s]

instance (ToNoun a,ToNoun b,ToNoun c,ToNoun d,ToNoun e) => ToNoun (a,b,c,d,e)
  where toNoun(p,q,r,s,t) = mkRow[toNoun p,toNoun q,toNoun r,toNoun s,toNoun t]

instance (FromNoun a,FromNoun b)
    => FromNoun (a,b)
  where
    fromNoun n = do
      r <- getRawRow n
      guard (length r == 2)
      (,) <$> fromNoun (r!0)
          <*> fromNoun (r!1)

instance (FromNoun a,FromNoun b,FromNoun c)
    => FromNoun (a,b,c)
  where
    fromNoun n = do
      r <- getRawRow n
      guard (length r == 3)
      (,,) <$> fromNoun (r!0)
           <*> fromNoun (r!1)
           <*> fromNoun (r!2)

instance (FromNoun a,FromNoun b,FromNoun c,FromNoun d)
    => FromNoun (a,b,c,d)
  where
    fromNoun n = do
      r <- getRawRow n
      guard (length r == 4)
      (,,,) <$> fromNoun (r!0)
            <*> fromNoun (r!1)
            <*> fromNoun (r!2)
            <*> fromNoun (r!3)

instance ToNoun ByteString where
    toNoun = BAR

instance FromNoun ByteString where
    fromNoun (BAR n) = Just n
    fromNoun  _      = Nothing

instance ToNoun LByteString where
    toNoun = BAR . toStrict

instance FromNoun LByteString where
    fromNoun (BAR n) = Just (fromStrict n)
    fromNoun  _      = Nothing

instance ToNoun Text where
    toNoun = NAT . utf8Nat

instance FromNoun Text where
    fromNoun (NAT n) = either (const Nothing) Just (natUtf8 n)
    fromNoun _       = Nothing

getRawRow :: Fan -> Maybe (Vector Fan)
getRawRow (ROW xs) = Just xs
getRawRow _        = Nothing

getRawTable :: Fan -> Maybe (Map Fan Fan)
getRawTable (TAB m) = Just m
getRawTable _       = Nothing

getRawBar :: Fan -> Maybe ByteString
getRawBar (BAR b) = Just b
getRawBar _       = Nothing

instance ToNoun a => ToNoun (Vector a) where
    toNoun = ROW . (fmap toNoun)
instance FromNoun a => FromNoun (Vector a) where
    fromNoun n = getRawRow n >>= mapM fromNoun

-- | Since we are very unlikely to ever want actual noun linked-lists
-- at an API boundary, we represent lists as rows.
instance ToNoun a => ToNoun [a] where
    toNoun = toNoun . V.fromList

-- | Since we are very unlikely to ever want actual noun linked-lists
-- at an API boundary, we represent lists as rows.
instance FromNoun a => FromNoun [a] where
    fromNoun n = toList @(Vector a) <$> fromNoun n

instance (ToNoun k, ToNoun v) => ToNoun (Map k v) where
    toNoun = TAB . M.fromList . map (both toNoun toNoun) . M.toList
      where
        both f g (a, b) = (f a, g b)

instance (Ord k, FromNoun k, FromNoun v) => FromNoun (Map k v) where
    fromNoun n = do
      r <- getRawTable n
      pairs <- forM (M.toList r) $ \(k, v) -> do
        kf <- fromNoun k
        kv <- fromNoun v
        pure (kf, kv)
      pure $ M.fromList pairs

instance (ToNoun a) => ToNoun (Maybe a) where
    toNoun Nothing  = NAT 0
    toNoun (Just a) = (NAT 0) %% (toNoun a)

instance (FromNoun a) => FromNoun (Maybe a) where
    fromNoun (NAT 0) = Just Nothing
    fromNoun n       = do
        let (h, t) = boom n
        case h of
            NAT 0 -> Just <$> fromNoun t
            _     -> Nothing

-- Hack: inputs are cast to unsigned
instance ToNoun a => ToNoun (IntMap a) where
    toNoun = TAB . mapFromList . fmap f . mapToList
      where
        f (k,v) = (NAT (fromIntegral k), toNoun v)

instance FromNoun a => FromNoun (IntMap a) where
    fromNoun = \case
        TAB t -> fmap mapFromList
                   $ for (mapToList t) \(kF,vF) -> do
                       k::Word64 <- fromNoun kF
                       v         <- fromNoun vF
                       pure (fromIntegral k, v)
        _     -> Nothing
