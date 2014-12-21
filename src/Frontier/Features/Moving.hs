{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
module Frontier.Features.Moving
    (Component()
    ,feature) where

import Frontier.Feature
import Frontier.Feature.Action
import qualified Frontier.Feature.Entity as E
import Frontier.Feature.Qualifier

data Component a where
    PlayerCharacter     :: Component Object
    Blank               :: Component a
    Dummy               :: Component a

deriving instance Show (Component a)
deriving instance Eq (Component a)

feature :: Feature Component
feature = Feature{..} where
    componentFor    :: E.Seed b -> Component b
    componentFor    E.PlayerCharacter       = PlayerCharacter
    componentFor    E.Blank                 = Blank
    componentFor    _                       = Dummy

    initItems       :: [E.Seed Item]
    initItems                               = []

    symbol          :: Component Object -> String
    symbol          PlayerCharacter         = "@"
    symbol          _                       = ""

    command         :: Char -> (Action Component -> c) -> [c]
    command         'h'         fn          = (:[]) . fn $ do
        shortDescription "Move east"
        me >>= move E
    command         'j'         fn          = (:[]) . fn $ do
        shortDescription "Move south"
        me >>= move S
    command         'k'         fn          = (:[]) . fn $ do
        shortDescription "Move north"
        me >>= move N
    command         'l'         fn          = (:[]) . fn $ do
        shortDescription "Move west"
        me >>= move W
    command         _           _           = []

    doTurn          :: Component Object -> (Action Component -> c) -> [c]
    doTurn          _           _           = []

    eq              :: Component a -> Component a -> Bool
    eq              Blank       _           = True
    eq              _           Blank       = True
    eq              a           b           = a == b

    partialUpdate   :: Component a -> Component a -> Component a
    partialUpdate   Blank       x           = x
    partialUpdate   x           _           = x
