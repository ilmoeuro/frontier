{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Farming
    (Thing()
    ,feature
    ) where

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
    symbol _ = '?'

    command :: Char -> ActionM Thing ()
    command _ = disabled

    initPlayerCharacter :: Thing Object
    initPlayerCharacter = PlayerCharacter
