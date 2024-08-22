-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module PlunderPrelude
    ( module X
    , writeTQueue'
    , writeTBQueue'
    , turn
    , whenJust
    , guarded
    , liftMaybe
    , mapMaybeA
    , pass
    , (!), (.!)
    )
where

import ClassyPrelude             as X hiding (trace, traceM, traceShowId)
import Control.Monad.Except      as X (MonadError(..), liftEither)
import Control.Monad.STM         as X (check, retry)
import Control.Concurrent.STM.TQueue as X (flushTQueue)
import Data.Coerce               as X (coerce)
import Data.Function             as X ((&))
import Data.List.NonEmpty        as X (NonEmpty(..))
import Data.Primitive.Array      as X hiding (fromList)
import Data.Primitive.SmallArray as X hiding (fromList)
import Data.Void                 as X (Void, absurd)
import Debug.Trace               as X (trace, traceM, traceShowId)
import Nat                       as X
import Optics                    as X (_1, _2, assign, assign', at, modifying,
                                       modifying', over, to, use, view, (%),
                                       (^.))
import Optics.TH                 as X
import System.IO.Unsafe          as X (unsafePerformIO)
import Text.Show.Pretty          as X (pPrint, ppShow)

--------------------------------------------------------------------------------

writeTQueue' :: TQueue a -> a -> STM ()
writeTQueue' a b = writeTQueue a $! b

writeTBQueue' :: TBQueue a -> a -> STM ()
writeTBQueue' a b = writeTBQueue a $! b

turn :: Functor f => f a -> (a -> b) -> f b
turn = (<&>)

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing  _   = pure ()
whenJust (Just x) act = act x

-- From Protolude, Relude, ...
guarded :: Alternative f => (a -> Bool) -> a -> f a
guarded p a = if p a then pure a else empty

liftMaybe :: Alternative f => Maybe a -> f a
liftMaybe Nothing  = empty
liftMaybe (Just a) = pure a

-- | Applicative 'mapMaybe'.
mapMaybeA :: Applicative f => (a -> f (Maybe b)) -> [a] -> f [b]
mapMaybeA f = fmap catMaybes . traverse f

type instance Element (Array a) = a
type instance Element (SmallArray a) = a

-- All methods default to Foldable methods, which is what we want.
instance MonoFoldable (Array a) where
instance MonoFoldable (SmallArray a) where

pass :: Monad m => m ()
pass = pure ()

(!) :: Array a -> Int -> a
(!) = indexArray

(.!) :: SmallArray a -> Int -> a
(.!) = indexSmallArray
