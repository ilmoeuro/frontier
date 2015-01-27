{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontier.Model.Core.Testing.Dynamic
    (env
    ) where

import Control.Lens
import Data.Function
import Frontier.Model.Core.Feature hiding (command, init, step)
import Frontier.Model.Core.Testing.Static
import Prelude hiding (init)

_entities :: Witness b -> Lens' (World c) [Entity c b]
_entities Object = _objects
_entities Item = _items

env :: forall c. (forall b. Tag b -> c b) -> Env (World c) (Entity c)
env fromTag = Env {..} where
    create :: Witness b -> Tag b -> (Entity c b -> Entity c b) -> Action (World c)
    create wit s fn w@World{lastUid}
        = (_entities wit %~ (++ [fn . seed wit lastUid fromTag $ s]))
        . (_lastUid +~ 1)
        $ w

    withAll :: Witness b -> ([Entity c b] -> Action (World c)) -> Action (World c)
    withAll Object act w = act (objects w) w
    withAll Item   act w = act (items w) w

    modify :: Witness b -> (Entity c b -> Entity c b) -> Entity c b -> Action (World c)
    modify wit fn e = _entities wit . each . filtered (`is` e) %~ fn

    destroy :: Witness b -> Entity c b -> Action (World c)
    destroy wit e = _entities wit %~ filter (not . (`is` e))

    is :: Entity c b -> Entity c b -> Bool
    is = (==) `on` uid

    withInitParam :: (Int -> Action (World c)) -> Action (World c)
    withInitParam act w = act (initParam w) w

    message :: ([String] -> [String]) -> Action (World c)
    message = (_messages %~)

    _position :: Lens' (Entity c Object) (Int, Int)
    _position = _meta . __position

    _symbol :: Lens' (Entity c Object) Char
    _symbol = _meta . __symbol

    _size :: Lens' (Entity c Object) Size
    _size = _meta . __size

    _tag :: Lens' (Entity c b) (Tag b)
    _tag = _entityTag
