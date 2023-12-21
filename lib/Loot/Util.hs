{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Loot.Util where

import PlunderPrelude
import Loot.Types
import Rex

import qualified Fan         as Fan
import qualified Fan.PlanRex as Fan

--------------------------------------------------------------------------------

lootRexToRex :: LootRex XVal -> GRex XVal
lootRexToRex = \case
    EVIL x       -> wrap "�" x
    EMBD x       -> wrap "▣" x
    NODE t r s h -> N t r (lootRexToRex <$> s) (lootRexToRex <$> h)
    LEAF t r h   -> T t r (lootRexToRex <$> h)
  where
    wrap ryn x@XVNAT{} = N PREF ryn [C x] Nothing
    wrap ryn x@XVBAR{} = N PREF ryn [C x] Nothing
    wrap ryn x@XVCOW{} = N PREF ryn [C x] Nothing
    wrap ryn x@XVREF{} = N PREF ryn [C x] Nothing
    wrap ryn x         = N OPEN ryn [C x] Nothing

lootRexToPex :: LootRex Fan.Any -> Fan.PlanRex
lootRexToPex = \case
    EVIL x       -> Fan.EVIL x
    EMBD x       -> Fan.EMBD x
    NODE t r s h -> Fan.NODE t r (lootRexToPex <$> s) (lootRexToPex <$> h)
    LEAF t r h   -> Fan.LEAF t r (lootRexToPex <$> h)
