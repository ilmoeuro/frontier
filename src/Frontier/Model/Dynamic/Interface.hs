module Frontier.Model.Dynamic.Interface
    (FrontierM
    ,Input(..)
    ,Output(..)
    ,display
    ) where

import Control.Monad.State.Strict
import Frontier.Model.Static
import Pipes

type FrontierM = Pipe Input Output (State World)

data Input
    = Move Direction
    | Chop Direction
    | Build Direction
    | Smash Direction
    | Quit

data Output
    = Display World

display :: World -> FrontierM ()
display = yield . Display
