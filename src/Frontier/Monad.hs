module Frontier.Monad
    (Input(..)
    ,Output(..)
    ,FrontierM
    ) where

import Control.Monad.State
import Frontier.Data
import Pipes

data Input
    = Load World
    | Move Direction
data Output
    = Save World

type FrontierM = Pipe Input Output (State World)
