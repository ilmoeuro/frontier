{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Building
    (Thing()
    ,feature
    ) where

import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier

data Thing a where
    PlayerCharacter     :: Thing Object
    Wall                :: Thing Object
    Tree                :: Thing Object
    Lumber              :: Thing Item
    Planks              :: Thing Item
    Saw                 :: Thing Item
    Hammer              :: Thing Item
    Axe                 :: Thing Item

deriving instance Show (Thing a)
deriving instance Eq (Thing a)

feature :: Feature Thing
feature = Feature {..} where

    initItems :: [Thing Item]
    initItems = [Saw, Hammer, Axe]

    symbol :: Thing Object -> Char
    symbol Wall             = '#'
    symbol Tree             = '^'
    symbol PlayerCharacter  = '@'

    action :: Char -> ActionM Thing ()
    action 's' = do
            requireItem NoConsume Saw
            item <- targetInventoryItem
            when (item /= Lumber) disableAction
            replaceTargetItem Planks
    action 'b' = do
            requireItem NoConsume Hammer
            requireItem Consume Planks
            targetEmptySpace
            replaceTargetObject Wall
    action 'c' = do
            requireItem NoConsume Axe
            object <- targetObject Near
            when (object /= Tree) disableAction
            yieldInventoryItem Lumber
            destroyTargetObject
    action _ = disableAction

    initPlayerCharacter :: Thing Object
    initPlayerCharacter = PlayerCharacter
