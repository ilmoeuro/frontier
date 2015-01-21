{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Model.Features.Building
    (Component()
    ,seed
    ,feature
    ) where

import Control.Lens hiding (Action)
import Frontier.Model.Feature hiding (PlayerCharacter)
import Prelude hiding (init)
import System.Random

data Component a where
    Unknown                     :: Component a

seed :: Seed b -> Component b
seed _                      = Unknown

feature :: forall w e. Feature Component w e
feature = Feature {..} where

    init :: Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    init Env{..} _ =
        foldr
            ((.) . mkTree)
            id
            $ zip (randoms (mkStdGen 0))
                  (randoms (mkStdGen 1))
      where
        mkTree (x', y') =
            create Object Opaque
                ((_position     .~ (x' `rem` 80, y' `rem` 24))
                .(_symbol       .~ '^'))

    command :: Char -> Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    command _ _ _ = id

    step :: Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    step _ _ = id
