-- Copyright 2023 The Plunder Authors
-- Use of this source code is governed by a BSD-style license that can be
-- found in the LICENSE file.

module Server.Common where

import PlunderPrelude

import Optics (set)

--------------------------------------------------------------------------------

data Pool a = POOL
    { tab :: IntMap a
    , nex :: Int
    }

makeFieldLabelsNoPrefix ''Pool

emptyPool :: Pool a
emptyPool = POOL mempty 1

poolRegister :: TVar (Pool a) -> a -> STM Int
poolRegister var val = do
    pool <- readTVar var
    let key = pool.nex
    let tab = insertMap key val pool.tab
    writeTVar var (POOL tab (key+1))
    pure key

poolUnregister :: TVar (Pool a) -> Int -> STM ()
poolUnregister tvar k = do
   modifyTVar' tvar (over #tab (deleteMap k))
