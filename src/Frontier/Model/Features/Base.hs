{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Model.Features.Base
    (Component()
    ,seed
    ,feature
    ) where

import Control.Lens hiding (Action)
import Frontier.Model.Feature hiding (PlayerCharacter)
import qualified Frontier.Model.Feature as Ftr
import Prelude hiding (init)

data Component a where
    PlayerCharacter             :: Component Object
    Unknown                     :: Component a

seed :: Seed b -> Component b
seed Ftr.PlayerCharacter    =  PlayerCharacter
seed _                      =  Unknown

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
    command c Env{..} _com = case c of
        'h' -> forEach Object (modify' (_position._1 -~ 1))
        'j' -> forEach Object (modify' (_position._2 +~ 1))
        'k' -> forEach Object (modify' (_position._2 -~ 1))
        'l' -> forEach Object (modify' (_position._1 +~ 1))
        _   -> id
      where
        modify' f e | PlayerCharacter <- e ^# _com = modify Object f e
        modify' _ _                                = id

    step :: Env w e -> (forall b. ALens' (e b) (Component b)) -> Action w
    step _ _ = id
