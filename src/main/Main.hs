{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Managed
import Data.Default
import Frontier.Controller
import Frontier.Model.Interaction hiding (Input, Output)
import qualified Frontier.Model.Interaction as Model
import Frontier.View
import Graphics.Vty
import MVC hiding (Input, Output)

getVty :: Managed Vty
getVty = managed $ \inside -> do
    vty@Vty{shutdown} <- mkVty def
    result <- inside vty
    shutdown
    return result

io :: Managed (View Model.Output, Controller Model.Input)
io = getVty >>= \Vty{inputIface, outputIface} ->
    let Input{_eventChannel} = inputIface
        controller = keyboardController _eventChannel
        view =  fmap (handles _Display) (spritesView outputIface)
             <> fmap (handles _Message) (messageView outputIface)
    in (,) <$> view <*> controller

main :: IO ()
main = void $ runMVC mkModelState (asPipe model) io
