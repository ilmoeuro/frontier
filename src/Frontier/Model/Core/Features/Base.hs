{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Model.Core.Features.Base
    (Component()
    ,seed
    ,feature
    ) where

import Control.Lens hiding (Action)
import Frontier.Model.Core.Feature hiding (PlayerCharacter)
import qualified Frontier.Model.Core.Feature as Ftr
import Prelude hiding (init)

data Component a where
    PlayerCharacter             :: Component Object
    Unknown                     :: Component a

seed :: Seed b -> Component b
seed Ftr.PlayerCharacter    =  PlayerCharacter
seed _                      =  Unknown

compose :: [a -> a] -> a -> a
compose = foldr (.) id

feature :: forall w e. Feature Component w e
feature = Feature {..} where

    init :: Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    init Env{..} _com
        = create
            Object
            Ftr.PlayerCharacter
            ((_position     .~ (1,1))
            .(_symbol       .~ '@'))

    command :: Char -> Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    command c Env{..} _com =
        withAll Object $ compose . \objs ->
            [ modify Object move obj
            | obj <- objs
            , isPC obj
            , and
                [position (move obj) /= position obj'
                | obj' <- objs
                , not (obj `is` obj')
                ]
            ]
      where
        position = view _position
        isPC obj | PlayerCharacter <- obj ^# _com   = True
        isPC _                                      = False
        move = case c of
            'h' -> _position._1 -~ 1
            'j' -> _position._2 +~ 1
            'k' -> _position._2 -~ 1
            'l' -> _position._1 +~ 1
            _   -> id

    step :: Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    step _ _ = id
