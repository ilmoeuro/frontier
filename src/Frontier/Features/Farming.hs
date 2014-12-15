{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Farming
    (Specific()
    ,feature
    ) where

import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier

data Specific a where
    PlayerCharacter     :: Specific Object

deriving instance Show (Specific a)
deriving instance Eq (Specific a)

feature :: Feature Specific
feature = Feature {..} where

    initItems :: [Specific Item]
    initItems = []

    symbol :: Specific Object -> Char
    symbol _ = '?'

    command :: Char -> ActionM Specific ()
    command _ = disabled

    initPlayerCharacter :: Specific Object
    initPlayerCharacter = PlayerCharacter

    eq :: Specific a -> Specific a -> Bool
    eq = (==)
