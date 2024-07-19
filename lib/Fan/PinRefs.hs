{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wall        #-}
{-# OPTIONS_GHC -Werror      #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fan.PinRefs where

import Control.Monad.State.Strict
import Data.Sorted
import Fan.Types
import PlunderPrelude

import qualified Data.Vector as V
import qualified Data.Set    as S

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
        KLO _ v -> traverse_ go v
        FUN f   -> go f.body
        SET c   -> traverse_ go (ssetToAscArray c)
        TAb t   -> do traverse_ go (tabKeysArray t)
                      traverse_ go (tabElemsArray t)
        PIN p -> do
            (acc, hs) <- get
            unless (member p.hash hs) do
                let !acc2 = p : acc
                let !hs2  = S.insert p.hash hs
                put $! (acc2, hs2)
