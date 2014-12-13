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

    action :: Char -> ActionM Thing ()
    action 'h' = void $ me >>= move W
    action 'j' = void $ me >>= move N
    action 'k' = void $ me >>= move S
    action 'l' = void $ me >>= move E
    action _   = disableAction

    initPlayerCharacter :: Thing Object
    initPlayerCharacter = PlayerCharacter
