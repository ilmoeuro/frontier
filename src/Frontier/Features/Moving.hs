{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Moving
    (Thing()
    ,feature) where

import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier

data Thing a where
    PlayerCharacter     :: Thing Object

deriving instance Show (Thing a)
deriving instance Eq (Thing a)

feature :: Feature Thing
feature = Feature {..} where

    initItems :: [Thing Item]
    initItems = []

    symbol :: Thing Object -> Char
    symbol PlayerCharacter = '@'

    command :: Char -> ActionM Thing ()
    command 'h' = void $ me >>= move W
    command 'j' = void $ me >>= move N
    command 'k' = void $ me >>= move S
    command 'l' = void $ me >>= move E
    command _   = disabled

    initPlayerCharacter :: Thing Object
    initPlayerCharacter = PlayerCharacter
