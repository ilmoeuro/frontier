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
    PlayerCharacter     :: Specific Object

deriving instance Show (Specific a)
deriving instance Eq (Specific a)

feature :: Feature Specific
feature = Feature {..} where

    initItems :: [Specific Item]
    initItems = []

    symbol :: Specific Object -> Char
    symbol PlayerCharacter = '@'

    move' :: Direction -> ActionM Specific ()
    move' dir =
        void $ shortDescription ("Move " ++ show dir) >> me >>= move dir

    command :: Char -> ActionM Specific ()
    command 'h' = move' W
    command 'j' = move' N
    command 'k' = move' S
    command 'l' = move' E
    command _   = disabled

    initPlayerCharacter :: Specific Object
    initPlayerCharacter = PlayerCharacter

    eq :: Specific a -> Specific a -> Bool
    eq = (==)
