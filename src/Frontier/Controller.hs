{-# LANGUAGE LambdaCase #-}
module Frontier.Controller
    (keyboardInput
    ) where

import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Managed
import qualified Frontier.Model.Dynamic.Interface as Dy
import qualified Frontier.Model.Static as St
import Graphics.Vty
import MVC hiding (Input)
import MVC.Prelude

data KeyIn = Repeat Int | Dir St.Direction | Cmd Char | Quit | None

keyboardInput :: Chan Event -> Managed (Controller Dy.Input)
keyboardInput events =
    let readKey = liftIO (readChan events) >>= \case
            EvKey (KChar 'h') _ -> return . Dir $ St.W
            EvKey (KChar 'j') _ -> return . Dir $ St.N
            EvKey (KChar 'k') _ -> return . Dir $ St.S
            EvKey (KChar 'l') _ -> return . Dir $ St.E
            EvKey (KChar c)   _ | c `elem` ['0'..'9']
                                -> return . Repeat $ read (c:"")
            EvKey (KChar c)   _ -> return . Cmd $ c
            EvKey KEsc        _ -> return Quit
            _                   -> return None
        pass = return ()
    in producer Single . forever $ readKey >>= \case
            (Repeat n)          -> readKey >>= \case
                (Dir d)         -> replicateM_ n $ yield (Dy.Move d)
                (Repeat m)      -> readKey >>= \case
                    (Dir d)     -> replicateM_ (n*10+m) $ yield (Dy.Move d)
                    _           -> pass
                _               -> pass
            (Dir d)             -> yield (Dy.Move d)
            (Cmd 'c')           -> readKey >>= \case
                (Dir d)         -> yield (Dy.Chop d)
                _               -> pass
            (Cmd 'b')           -> readKey >>= \case
                (Dir d)         -> yield (Dy.Build d)
                _               -> pass
            (Cmd 's')           -> readKey >>= \case
                (Dir d)         -> yield (Dy.Smash d)
                _               -> pass
            Quit                -> yield Dy.Quit
            _                   -> pass
