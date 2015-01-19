module Frontier.Model.Dynamic.Interface
    (FrontierM
    ,Input(..)
    ,Output(..)
    ,display
    ,message
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
    | Query Direction
    | Unbox Direction
    | Quit

data Output
    = Display World
    | Message String World

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

display :: World -> FrontierM ()
display = yield . Display

message :: String -> World -> FrontierM ()
message = yield .: Message
