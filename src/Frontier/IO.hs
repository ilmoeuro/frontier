{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Frontier.IO
    (io
    ) where

import Control.Applicative
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Managed
import Data.Default
import Data.Map (assocs)
import qualified Frontier.Model.Dynamic.Interface as Dy
import Frontier.Model.Static (World (..), objects, playerCharacter)
import qualified Frontier.Model.Static as St
import Graphics.Vty
import MVC hiding (Input)
import MVC.Prelude

getVty :: Managed Vty
getVty = managed $ \inside -> do
    vty@Vty{shutdown} <- mkVty def
    result <- inside vty
    shutdown
    return result

symbol :: St.Object -> Char
symbol St.Wall = '#'
symbol St.Tree = '^'
symbol St.PlayerCharacter = '@'

data KeyIn = Dir St.Direction | Cmd Char | Quit | None

keyboardInput :: Chan Event -> Managed (Controller Dy.Input)
keyboardInput events =
    let readKey = liftIO (readChan events) >>= \case
            EvKey (KChar 'h') _ -> return . Dir $ St.W
            EvKey (KChar 'j') _ -> return . Dir $ St.N
            EvKey (KChar 'k') _ -> return . Dir $ St.S
            EvKey (KChar 'l') _ -> return . Dir $ St.E
            EvKey (KChar c)   _ -> return . Cmd $ c
            EvKey KEsc        _ -> return Quit
            _                   -> return None
        pass = return ()
    in producer Single . forever $ readKey >>= \case
            (Dir d)                 -> yield (Dy.Move d)
            (Cmd 'c')               -> readKey >>= \case
                (Dir d)             -> yield (Dy.Chop d)
                _                   -> pass
            (Cmd 'b')               -> readKey >>= \case
                (Dir d)             -> yield (Dy.Build d)
                _                   -> pass
            Quit                    -> yield Dy.Quit
            _                       -> pass

vtyOutput :: (Picture -> IO ()) -> Managed (View Dy.Output)
vtyOutput update = consumer . forever $ await >>= \case
    (Dy.Display (World{playerCharacter,objects})) ->
        liftIO
        . update
        . picForLayers
        $ layers
      where
        ((px,py),pobj) = playerCharacter
        pcLayer = translate px py
                . char def
                $ symbol pobj
        objLayers = map $ \((x,y),obj) -> translate x y
                                        . char def
                                        . symbol
                                        $ obj
        layers = pcLayer : objLayers (assocs objects)

io :: Managed (View Dy.Output, Controller Dy.Input)
io = getVty >>= \Vty{inputIface,update} ->
    let Input{_eventChannel} = inputIface
        controller = keyboardInput _eventChannel
        view = vtyOutput update
    in (,) <$> view <*> controller
