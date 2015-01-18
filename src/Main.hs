module Main where

import Frontier.IO (io)
import Frontier.Model.Dynamic (run)
import Frontier.Model.Dynamic.Interface (Input, Output)
import Frontier.Model.Static (World, defaultWorld)
import MVC hiding (Input, Output)

model :: Model World Input Output
model = asPipe run

main :: IO ()
main = void $ runMVC defaultWorld model io
