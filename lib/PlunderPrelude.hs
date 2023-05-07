-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module PlunderPrelude
    ( module X
    , writeTQueue'
    , writeTBQueue'
    , turn
    , whenJust
    , pass
    )
where

import ClassyPrelude             as X hiding (trace, traceM, traceShowId)
import Control.Monad.Except      as X (MonadError(..), liftEither)
import Control.Monad.STM         as X (retry)
import Data.Coerce               as X (coerce)
import Data.Function             as X ((&))
import Data.List.NonEmpty        as X (NonEmpty(..))
import Data.Primitive.SmallArray as X
import Data.Void                 as X (Void, absurd)
import Debug.Trace               as X (trace, traceM, traceShowId)
import Natty                     as X
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

-- All methods default to Foldable methods, which is what we want.
instance MonoFoldable (SmallArray a) where
type instance Element (SmallArray a) = a

pass :: Monad m => m ()
pass = pure ()
