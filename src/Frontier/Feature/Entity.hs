{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Feature.Entity
    (Seed(..)
    ) where

import Frontier.Feature.Qualifier

data Seed a where
    PlayerCharacter     :: Seed Object
    Wall                :: Seed Object
    Tree                :: Seed Object
    Lumber              :: Seed Item
    Planks              :: Seed Item
    Saw                 :: Seed Item
    Hammer              :: Seed Item
    Axe                 :: Seed Item
    Blank               :: Seed a
    Opaque              :: Seed a

deriving instance Show (Seed a)
deriving instance Eq (Seed a)
