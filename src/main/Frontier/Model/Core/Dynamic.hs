{-# LANGUAGE GADTs           #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Model.Core.Dynamic
    (init
    ,command
    ,step
    ,loadLevel
    ) where

import Control.Lens
import Data.Function
import Frontier.Model.Core.Feature hiding (command, init, step, loadLevel)
import qualified Frontier.Model.Core.Feature as Ftr
import qualified Frontier.Model.Core.Features.Base as Base
import qualified Frontier.Model.Core.Features.Building as Building
import Frontier.Model.Core.Static
import Prelude hiding (init)

_entities :: Witness b -> Lens' World [Entity b]
_entities Object = _objects
_entities Item = _items

env :: Env World Entity
env = Env {..} where

    dirtify :: Witness b -> Entity b -> Action World
    dirtify wit entity = case wit of
                Object -> _dirtyTiles %~ (entity ^. _position :)
                _      -> id

    create :: Witness b -> Tag b -> (Entity b -> Entity b) -> Action World
    create wit s fn w@World{lastUid}
        = (_entities wit %~ (++ [entity]))
        . (_lastUid +~ 1)
        . dirtify wit entity
        $ w
      where
        entity = fn . seed wit lastUid $ s

    withAll :: Witness b -> ([Entity b] -> Action World) -> Action World
    withAll Object act w = act (objects w) w
    withAll Item   act w = act (items w) w

    modify :: Witness b -> (Entity b -> Entity b) -> Entity b -> Action World
    modify wit fn e = dirtify wit e
                    . dirtify wit (fn e)
                    . (_entities wit . each . filtered (`is` e) %~ fn)

    destroy :: Witness b -> Entity b -> Action World
    destroy wit e = dirtify wit e
                  . (_entities wit %~ filter (not . (`is` e)))

    is :: Entity b -> Entity b -> Bool
    is = (==) `on` uid

    withInitParam :: (Int -> Action World) -> Action World
    withInitParam act w = act (initParam w) w

    message :: ([String] -> [String]) -> Action World
    message = (_messages %~)

    _position :: Lens' (Entity Object) (Int, Int)
    _position = _meta . __position

    _symbol :: Lens' (Entity Object) Char
    _symbol = _meta . __symbol

    _zIndex :: Lens' (Entity Object) Int
    _zIndex = _meta . __zIndex

    _tag :: Lens' (Entity b) (Tag b)
    _tag = _entityTag

universal
    :: (forall w.
           Feature w
        -> Action w)
    -> Action World
universal f
    = f (Base.feature env (_components._base))
    . f (Building.feature env (_components._building))

init :: Action World
init = universal Ftr.init

command :: String -> Action World
command c = universal (`Ftr.command` c)

step :: Action World
step = universal Ftr.step

loadLevel :: LevelSource -> Action World
loadLevel levelSource = universal (`Ftr.loadLevel` levelSource)
