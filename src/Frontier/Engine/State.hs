{-# LANGUAGE TemplateHaskell #-}
module Frontier.Engine.State
    (EngineState(..)
    ,_worldSize
    ,_world
    ,_playerCharacter
    ) where

import Control.Lens.TH
import Frontier.Feature.Qualifier

data EngineState a = EngineState
    {worldSize          :: (Int, Int)
    ,world              :: [((Int, Int), a Object)]
    ,playerCharacter    :: ((Int, Int), a Object)
    }

makeLensesFor
    [("worldSize"           ,"_worldSize")
    ,("world"               ,"_world")
    ,("playerCharacter"     ,"_playerCharacter")
    ]
    ''EngineState
