{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Feature.Entity
    (Seed(..)
    ) where

import Frontier.Feature.Qualifier

data Seed a where
    Opaque              :: Seed a
    PlayerCharacter     :: Seed Object
    Saw                 :: Seed Item
    Hammer              :: Seed Item
    Axe                 :: Seed Item

deriving instance Show (Seed a)
deriving instance Eq (Seed a)