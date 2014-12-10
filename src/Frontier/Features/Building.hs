{-# LANGUAGE GADTs #-}
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
    = Wall
    | Tree
    deriving (Eq, Show)

data Item
    = Lumber
    | Planks
    | Saw
    | Hammer
    | Axe
    deriving (Eq, Show)

feature :: Feature Object Item
feature = Feature{..} where

    use :: Item -> ActionM Item Object ()
    use Saw = do
            item <- targetInventoryItem
            when (item /= Lumber) disableAction
            replaceTargetItem Planks
    use Hammer = do
            consumeItem Planks
            targetEmptySpace
            replaceTargetObject Wall
    use Axe = do
            object <- targetNearObject
            when (object /= Tree) disableAction
            yieldInventoryItem Lumber
            destroyTargetObject
    use _ = disableAction

    initialItems :: [Item]
    initialItems = [Saw, Hammer, Axe]

    symbol :: Object -> Char
    symbol Wall = '#'
    symbol Tree = '^'

    objectTag    = const Nothing
    itemTag      = const Nothing
    tagObject    = const Nothing
    tagItem      = const Nothing