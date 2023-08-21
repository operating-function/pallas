{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fan.PinRefs where

import Fan.Types
import PlunderPrelude
import Control.Monad.State.Strict

import qualified Data.Vector as V
import qualified Data.Set    as S
import qualified Data.Map    as M

--------------------------------------------------------------------------------

-- TODO: Is there really no `V.fromListReverseN`?  Should be
-- easy to implement efficiently, and it's a supremely common
-- operation.

pinRefs :: Any -> Vector Pin
pinRefs top =
    let (pins, hashes) = execState (go top) (mempty, mempty)
    in V.fromListN (S.size hashes) (reverse pins)
  where
    go :: Any -> State ([Pin], Set Hash256) ()
    go = \case
        NAT{}   -> pure ()
        COw{}   -> pure ()
        BAR{}   -> pure ()
        ROW v   -> traverse_ go v
        REX v   -> traverse_ go v
        KLO _ v -> traverse_ go v
        FUN f   -> go f.body
        SET c   -> traverse_ go (S.toList c)
        TAb t   -> traverse_ go (M.keys t) >> traverse_ go (M.elems t)
        PIN p -> do
            (acc, hs) <- get
            unless (member p.hash hs) do
                let !acc2 = p : acc
                let !hs2  = S.insert p.hash hs
                put $! (acc2, hs2)
