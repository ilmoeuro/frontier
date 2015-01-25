{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontier.Model.Core.Features.Base
    (Component()
    ,seed
    ,feature
    ) where

import Control.Lens hiding (Action)
import Frontier.Model.Core.Feature hiding (PlayerCharacter)
import qualified Frontier.Model.Core.Feature as Ftr
import Frontier.Model.Core.Feature.Prelude
import Prelude hiding (init)

data Component a where
    PlayerCharacter             :: Component Object
    Unknown                     :: Component a

seed :: Seed b -> Component b
seed Ftr.PlayerCharacter    =  PlayerCharacter
seed _                      =  Unknown

compose :: [a -> a] -> a -> a
compose = foldr (.) id

feature :: forall e w. Env w e
        -> (forall b. ALens' (e b) (Component b))
        -> Feature w
feature env@Env{..} _com = Feature {..} where

    init :: Action w
    init = create
            Object
            Ftr.PlayerCharacter
            ((_position     .~ (1,1))
            .(_symbol       .~ '@'))

    command :: String -> Action w
    command c | c `elem` ["h", "j", "k", "l"] =
        withAll Object $ compose . \objs ->
            [ modify Object move obj
            | obj <- objs
            , isPC obj
            , noCollision env (move obj) objs
            ]
      where
        isPC obj | PlayerCharacter <- obj ^# _com   = True
        isPC _                                      = False
        move = case c of
            "h" -> _position._1 -~ 1
            "j" -> _position._2 +~ 1
            "k" -> _position._2 -~ 1
            "l" -> _position._1 +~ 1
            _   -> id
    command _ = id

    step :: Action w
    step = id
