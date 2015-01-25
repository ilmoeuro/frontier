{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
module Frontier.Model.Core.Features.Building
    (Component()
    ,seed
    ,feature
    ) where

import Control.Lens hiding (Action, contains)
import Frontier.Model.Core.Feature hiding (PlayerCharacter)
import qualified Frontier.Model.Core.Feature as Ftr
import Frontier.Model.Core.Feature.Prelude
import Prelude hiding (init)
import System.Random

data Component a where
    PlayerCharacter             :: Component Object
    Hammer                      :: Component Item
    Axe                         :: Component Item
    Lumber                      :: Component Item
    Unknown                     :: Component a

deriving instance Eq (Component a)

compose :: [a -> a] -> a -> a
compose = foldr (.) id

seed :: Seed b -> Component b
seed Ftr.PlayerCharacter    =  PlayerCharacter
seed _                      =  Unknown

feature :: forall e w. Env w e
        -> (forall b. ALens' (e b) (Component b))
        -> Feature w
feature env@Env{..} _com = Feature {..} where

    init :: Action w
    init = foldr ((.) . mkTree) id
         . take 100
         $ zip (randoms (mkStdGen 0))
               (randoms (mkStdGen 1))
      where
        mkTree (x', y') =
            create Object Opaque
                ((_position     .~ (x' `rem` 80, y' `rem` 24))
                .(_symbol       .~ '^'))

    command :: String -> Action w
    command c | c `elem` ["bh", "bj", "bk", "bl"] = build where
        move = case c of
            "bh" -> _position._1 -~ 1
            "bj" -> _position._2 +~ 1
            "bk" -> _position._2 -~ 1
            "bl" -> _position._1 +~ 1
            _    -> id
        createWall = withAll Object $ compose . \objs ->
            [create Object Opaque
             ( move
             . (_position   .~ (obj ^. _position))
             . (_symbol     .~ '#'))
            | obj <- objs
            , isPC obj
            , noCollision env (move obj) objs
            ]
        lumbers = filter ((== Lumber) . (^# _com))
        contains es e = elem e . map (^# _com) $ es
        build = withAll Item $ \items ->
            when (items `contains` Hammer)
            $ case lumbers items of
                (lumber:_)  ->
                    destroy Item lumber
                    . createWall
                _ -> id
        isPC obj | PlayerCharacter <- obj ^# _com   = True
        isPC _                                      = False
    command _ = id

    step :: Action w
    step = id
