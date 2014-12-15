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
    NoOpAction          :: Specific (Action ())
    PlayerCharacter     :: Specific Object

deriving instance Eq (Specific a)
deriving instance Show (Specific a)

feature :: Feature Specific
feature = Feature {..} where

    initItems :: [Specific Item]
    initItems = []

    symbol :: Specific Object -> Char
    symbol _ = '?'

    command :: Char -> Specific (Action ())
    command _ = NoOpAction

    initPlayerCharacter :: Specific Object
    initPlayerCharacter = PlayerCharacter

    eq :: Specific a -> Specific a -> Bool
    eq = (==)

    run :: Specific (Action b) -> ActionM Specific b
    run NoOpAction = disabled
