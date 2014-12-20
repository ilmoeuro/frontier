{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Building
    (Component()
    ,feature
    ) where

import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action
import qualified Frontier.Feature.Entity as E
import Frontier.Feature.Qualifier

data Component a where
    Wall                :: Component Object
    Tree                :: Component Object
    Lumber              :: Component Item
    Planks              :: Component Item
    Saw                 :: Component Item
    Hammer              :: Component Item
    Axe                 :: Component Item
    Dummy               :: Component a

deriving instance Show (Component a)
deriving instance Eq (Component a)

feature :: Feature Component
feature = \case
    (ComponentFor entity)               -> case entity of
        E.Saw       -> Saw
        E.Hammer    -> Hammer
        E.Axe       -> Axe
        _           -> Dummy

    InitItems                           ->
        [E.Saw
        ,E.Hammer
        ,E.Axe
        ]

    (Symbol Wall)                       -> "#"
    (Symbol Tree)                       -> "^"
    (Symbol Dummy)                      -> ""

    (Command 's' fn)                    -> (:[]) . fn feature $ do
        shortDescription "Saw lumber"
        requireItem Saw
        target $ InventoryItem $ \item -> do
            guard (item == Lumber)
            replaceWith (Planks, E.Opaque)

    (Command 'b' fn)                    -> (:[]) . fn feature $ do
        shortDescription "Build a wall"
        requireItem Hammer
        consumeItem Planks
        target $ EmptySpace $ replaceWith (Wall, E.Opaque)

    (Command 'c' fn)                    -> (:[]) . fn feature $ do
        shortDescription "Chop down trees"
        requireItem Axe
        target $ NearObject $ \object -> do
            guard (object == Tree)
            yieldItem Lumber
            destroy

    (Command _ _)                       -> []

    (Eq a b)                            -> a == b
