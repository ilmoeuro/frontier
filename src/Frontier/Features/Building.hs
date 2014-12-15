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

    command :: Char -> ActionM Thing ()
    command 's' = do
            shortDescription "Saw lumber"
            requireItem NoConsume Saw
            item <- targetInventoryItem
            guard (item == Lumber)
            replaceTargetItem Planks
    command 'b' = do
            shortDescription "Build a wall"
            requireItem NoConsume Hammer
            requireItem Consume Planks
            targetEmptySpace
            replaceTargetObject Wall
    command 'c' = do
            shortDescription "Chop down trees"
            requireItem NoConsume Axe
            object <- targetObject Near
            guard (object == Tree)
            yieldInventoryItem Lumber
            destroyTargetObject
    command _ =
            disabled

    initPlayerCharacter :: Thing Object
    initPlayerCharacter = PlayerCharacter

    eq :: Thing a -> Thing a -> Bool
    eq = (==)
