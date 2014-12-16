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

    command :: Char -> Maybe (Specific (Action ()))
    command 'h' = Just $ MoveAction W
    command 'j' = Just $ MoveAction N
    command 'k' = Just $ MoveAction S
    command 'l' = Just $ MoveAction E
    command _   = Nothing

    initPlayerCharacter :: Specific Object
    initPlayerCharacter = PlayerCharacter

    eq :: Specific a -> Specific a -> Bool
    eq = (==)

    run :: Specific (Action b) -> ActionM Specific b
    run (MoveAction dir) =
        void $ shortDescription ("Move " ++ show dir) >> me >>= move dir
