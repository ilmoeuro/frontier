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

    move' :: Direction -> ActionM Thing ()
    move' dir =
        void $ shortDescription ("Move " ++ show dir) >> me >>= move dir

    command :: Char -> ActionM Thing ()
    command 'h' = move' W
    command 'j' = move' N
    command 'k' = move' S
    command 'l' = move' E
    command _   = disabled

    initPlayerCharacter :: Thing Object
    initPlayerCharacter = PlayerCharacter

    eq :: Thing a -> Thing a -> Bool
    eq = (==)
