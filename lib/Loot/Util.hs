{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Loot.Util where

import PlunderPrelude
import Loot.Types
import Rex

import qualified Fan         as Fan
import qualified Fan.PlanRex as Fan

--------------------------------------------------------------------------------

lootRexToRex :: LootRex a -> GRex a
lootRexToRex = \case
    EVIL x       -> N OPEN "�" [C x] Nothing
    EMBD x       -> N OPEN "▣" [C x] Nothing
    NODE t r s h -> N t r (lootRexToRex <$> s) (lootRexToRex <$> h)
    LEAF t r h   -> T t r (lootRexToRex <$> h)

lootRexToPex :: LootRex Fan.Any -> Fan.PlanRex
lootRexToPex = \case
    EVIL x       -> Fan.EVIL x
    EMBD x       -> Fan.EMBD x
    NODE t r s h -> Fan.NODE t r (lootRexToPex <$> s) (lootRexToPex <$> h)
    LEAF t r h   -> Fan.LEAF t r (lootRexToPex <$> h)
