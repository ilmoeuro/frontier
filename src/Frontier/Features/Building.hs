{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
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
    Dummy               :: Component a
    PlayerCharacter     :: Component Object
    Wall                :: Component Object
    Tree                :: Component Object
    Lumber              :: Component Item
    Planks              :: Component Item
    Saw                 :: Component Item
    Hammer              :: Component Item
    Axe                 :: Component Item

deriving instance Show (Component a)
deriving instance Eq (Component a)

feature :: Feature Component
feature = \case
    (ComponentFor E.Saw)            -> Saw
    (ComponentFor E.Hammer)         -> Hammer
    (ComponentFor E.Axe)            -> Axe
    (ComponentFor E.PlayerCharacter)-> PlayerCharacter

    InitItems                       -> [E.Saw
                                       ,E.Hammer
                                       ,E.Axe
                                       ]

    (Symbol Wall)                   -> "#"
    (Symbol Tree)                   -> "^"
    (Symbol PlayerCharacter)        -> "@"
    (Symbol Dummy)                  -> " "

    (Command 's' fn)                -> (:[]) . fn $ do
        shortDescription "Saw lumber"
        requireItem NoConsume Saw
        target $ InventoryItem $ \item -> do
            guard (item == Lumber)
            replaceWith Planks
    (Command 'b' fn)                -> (:[]) . fn $ do
        shortDescription "Build a wall"
        requireItem NoConsume Hammer
        requireItem Consume Planks
        target $ EmptySpace $ replaceWith Wall
    (Command 'c' fn)                -> (:[]) . fn $ do
        shortDescription "Chop down trees"
        requireItem NoConsume Axe
        target $ NearObject $ \object -> do
            guard (object == Tree)
            yieldItem Lumber
            destroy
    (Command _ fn)                  -> (:[]) . fn $ disabled

    (Eq a b)                        -> a == b
