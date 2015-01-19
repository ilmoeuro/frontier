{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Managed
import Data.Default
import Frontier.Controller (keyboardInput)
import Frontier.Model.Dynamic (run)
import qualified Frontier.Model.Dynamic.Interface as Dy
import Frontier.Model.Static (defaultWorld)
import Frontier.View (vtyOutput)
import Graphics.Vty
import MVC hiding (Input, Output)

getVty :: Managed Vty
getVty = managed $ \inside -> do
    vty@Vty{shutdown} <- mkVty def
    result <- inside vty
    shutdown
    return result

io :: Managed (View Dy.Output, Controller Dy.Input)
io = getVty >>= \Vty{inputIface,update} ->
    let Input{_eventChannel} = inputIface
        controller = keyboardInput _eventChannel
        view = vtyOutput update
    in (,) <$> view <*> controller

main :: IO ()
main = void $ runMVC defaultWorld (asPipe run) io
