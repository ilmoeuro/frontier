{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Building
    (Specific()
    ,feature
    ) where

import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier

data Specific a where
    NoOpAction          :: Specific (Action ())
    SawAction           :: Specific (Action ())
    BuildAction         :: Specific (Action ())
    ChopAction          :: Specific (Action ())
    PlayerCharacter     :: Specific Object
    Wall                :: Specific Object
    Tree                :: Specific Object
    Lumber              :: Specific Item
    Planks              :: Specific Item
    Saw                 :: Specific Item
    Hammer              :: Specific Item
    Axe                 :: Specific Item

deriving instance Show (Specific a)
deriving instance Eq (Specific a)

feature :: Feature Specific
feature = Feature {..} where

    initItems :: [Specific Item]
    initItems = [Saw, Hammer, Axe]

    symbol :: Specific Object -> Char
    symbol Wall             = '#'
    symbol Tree             = '^'
    symbol PlayerCharacter  = '@'

    run :: Specific (Action a) -> ActionM Specific a
    run SawAction = do
            shortDescription "Saw lumber"
            requireItem NoConsume Saw
            item <- targetInventoryItem
            guard (item == Lumber)
            replaceTargetItem Planks
    run BuildAction = do
            shortDescription "Build a wall"
            requireItem NoConsume Hammer
            requireItem Consume Planks
            targetEmptySpace
            replaceTargetObject Wall
    run ChopAction = do
            shortDescription "Chop down trees"
            requireItem NoConsume Axe
            object <- targetObject Near
            guard (object == Tree)
            yieldInventoryItem Lumber
            destroyTargetObject
    run NoOpAction = disabled

    command :: Char -> Specific (Action ())
    command 's' = SawAction
    command 'b' = BuildAction
    command 'c' = ChopAction
    command _   = NoOpAction

    initPlayerCharacter :: Specific Object
    initPlayerCharacter = PlayerCharacter

    eq :: Specific a -> Specific a -> Bool
    eq = (==)
