{-# LANGUAGE GADTs #-}
module Frontier.Feature.Entity
    (Entity(..)
    ) where

import Frontier.Feature.Qualifier

data Entity a where
    PlayerCharacter     :: Entity Object
    Saw                 :: Entity Item
    Hammer              :: Entity Item
    Axe                 :: Entity Item
