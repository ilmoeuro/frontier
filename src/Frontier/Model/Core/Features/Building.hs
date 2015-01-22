{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Model.Core.Features.Building
    (Component()
    ,seed
    ,feature
    ) where

import Control.Lens hiding (Action)
import Frontier.Model.Core.Feature hiding (PlayerCharacter)
import qualified Frontier.Model.Core.Feature as Ftr
import Prelude hiding (init)
import System.Random

data Component a where
    PlayerCharacter             :: Component Object
    Unknown                     :: Component a

compose :: [a -> a] -> a -> a
compose = foldr (.) id

seed :: Seed b -> Component b
seed Ftr.PlayerCharacter    =  PlayerCharacter
seed _                      =  Unknown

feature :: forall w e. Feature Component w e
feature = Feature {..} where

    init :: Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    init Env{..} _
        = foldr
            ((.) . mkTree)
            id
        . take 100
        $ zip (randoms (mkStdGen 0))
              (randoms (mkStdGen 1))
      where
        mkTree (x', y') =
            create Object Opaque
                ((_position     .~ (x' `rem` 80, y' `rem` 24))
                .(_symbol       .~ '^'))

    command :: String -> Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    command c Env{..} _com | c `elem` ["bh", "bj", "bk", "bl"] = build where
        dir | c == "bh" = _1 -~ 1
            | c == "bj" = _2 +~ 1
            | c == "bk" = _2 -~ 1
            | c == "bl" = _1 +~ 1
            | otherwise = id
        build = withAll Object $ compose . \objs ->
            [create Object Opaque
             ( (_position        %~ dir)
             . (_position        .~ (obj ^. _position))
             . (_symbol          .~ '#'))
            | obj <- objs
            , isPC obj
            ]
        isPC obj | PlayerCharacter <- obj ^# _com   = True
        isPC _                                      = False
    command _ _ _ = id

    step :: Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    step _ _ = id
