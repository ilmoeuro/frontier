{-# LANGUAGE RecordWildCards #-}
module Frontier.Features.Building
    (Object()
    ,Item()
    ,feature
    ) where

import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action

data Object
    = PlayerCharacter
    | Wall
    | Tree
    deriving (Eq, Show)

data Item
    = Lumber
    | Planks
    | Saw
    | Hammer
    | Axe
    deriving (Eq, Show)

feature :: Feature Item Object
feature = Feature {..} where

    initialItems :: [Item]
    initialItems = [Saw, Hammer, Axe]

    symbol :: Object -> Char
    symbol Wall             = '#'
    symbol Tree             = '^'
    symbol PlayerCharacter  = '@'

    action :: Char -> ActionM Item Object ()
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

    initPlayerCharacter :: Object
    initPlayerCharacter = PlayerCharacter
