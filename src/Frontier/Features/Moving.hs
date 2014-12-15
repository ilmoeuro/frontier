{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Moving
    (Specific()
    ,feature) where

import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier

data Specific a where
    NoOpAction          :: Specific (Action ())
    MoveAction          :: Direction -> Specific (Action ())
    PlayerCharacter     :: Specific Object

deriving instance Show (Specific a)
deriving instance Eq (Specific a)

feature :: Feature Specific
feature = Feature {..} where

    initItems :: [Specific Item]
    initItems = []

    symbol :: Specific Object -> Char
    symbol PlayerCharacter = '@'

    command :: Char -> Specific (Action ())
    command 'h' = MoveAction W
    command 'j' = MoveAction N
    command 'k' = MoveAction S
    command 'l' = MoveAction E
    command _   = NoOpAction

    initPlayerCharacter :: Specific Object
    initPlayerCharacter = PlayerCharacter

    eq :: Specific a -> Specific a -> Bool
    eq = (==)

    run :: Specific (Action b) -> ActionM Specific b
    run NoOpAction       = disabled
    run (MoveAction dir) =
        void $ shortDescription ("Move " ++ show dir) >> me >>= move dir
