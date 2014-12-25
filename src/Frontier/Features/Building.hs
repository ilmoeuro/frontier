{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Building
    (Component()
    ,feature
    ) where

import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action
import qualified Frontier.Feature.Entity as E
import Frontier.Feature.GenericMethods
import Frontier.Feature.Qualifier

data Component a where
    Wall                :: Component Object
    Tree                :: Component Object
    Lumber              :: Component Item
    Planks              :: Component Item
    Saw                 :: Component Item
    Hammer              :: Component Item
    Axe                 :: Component Item
    Blank               :: Component a
    Dummy               :: Component a

deriving instance Show (Component a)
deriving instance Eq (Component a)

feature :: Feature Component
feature = Feature{..} where
    componentFor    :: E.Seed b -> Component b
    componentFor    E.Saw                   = Saw
    componentFor    E.Hammer                = Hammer
    componentFor    E.Axe                   = Axe
    componentFor    E.Blank                 = Blank
    componentFor    _                       = Dummy

    initItems       :: [E.Seed Item]
    initItems                               = [E.Saw, E.Hammer, E.Axe]

    symbol          :: Component Object -> String
    symbol          Wall                    = "#"
    symbol          Tree                    = "^"
    symbol          _                       = ""

    command         :: Monad m => Char -> (ActionT Component m () -> c) -> [c]
    command         's'         fn          = (:[]) . fn $ do
        shortDescription "Saw lumber"
        requireItem Saw
        item <- targetItem
        guard (item == Lumber)
        replaceTargetItem (Planks, E.Opaque)
    command         'b'         fn          = (:[]) . fn $ do
        shortDescription "Build a wall"
        requireItem Hammer
        consumeItem Planks
        targetEmptySpace
        replaceTargetObject (Wall, E.Opaque)
    command         'c'         fn          = (:[]) . fn $ do
        shortDescription "Chop down trees"
        requireItem Axe
        obj <- targetObject
        guard (obj == Tree)
        yieldItem (Lumber, E.Opaque)
        destroyTargetObject
    command         _           _           = []

    doTurn          :: Component Object -> (ActionT Component m () -> c) -> [c]
    doTurn          _           _           = []

    eq              :: Component a -> Component a -> Bool
    eq                                      = genericEq Blank

    partialUpdate   :: Component a -> Component a -> Component a
    partialUpdate                           = genericPartialUpdate Blank
