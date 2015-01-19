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

data KeyIn
    = Repeat Int
    | Dir St.Direction
    | DirCmd (St.Direction -> Dy.Input)
    | Quit
    | None

mapKeyEvent :: Event -> KeyIn
mapKeyEvent (EvKey k _) = case k of
    (KChar 'h')   -> Dir St.W
    KLeft         -> Dir St.W
    (KChar 'j')   -> Dir St.S
    KDown         -> Dir St.S
    (KChar 'k')   -> Dir St.N
    KUp           -> Dir St.N
    (KChar 'l')   -> Dir St.E
    KRight        -> Dir St.E
    (KChar 'c')   -> DirCmd Dy.Chop
    (KChar 'b')   -> DirCmd Dy.Build
    (KChar 's')   -> DirCmd Dy.Smash
    (KChar 'u')   -> DirCmd Dy.Unbox
    (KChar '?')   -> DirCmd Dy.Query
    (KChar c)     | c `elem` ['0'..'9']
                  -> Repeat $ read [c]
    (KChar 'q')   -> Quit
    KEsc          -> Quit
    _             -> None
mapKeyEvent _           = None

keyboardInput :: Chan Event -> Managed (Controller Dy.Input)
keyboardInput events =
    let readKey = mapKeyEvent `liftM` liftIO (readChan events)
        pass = return ()
    in producer Single . forever $ readKey >>= \case
            (Repeat n)          -> readKey >>= \case
                (Dir d)         -> replicateM_ n $ yield (Dy.Move d)
                (Repeat m)      -> readKey >>= \case
                    (Dir d)     -> replicateM_ (n*10+m) $ yield (Dy.Move d)
                    _           -> pass
                _               -> pass
            (DirCmd c)          -> readKey >>= \case
                (Dir d)         -> yield $ c d
                _               -> pass
            (Dir d)             -> yield (Dy.Move d)
            Quit                -> yield Dy.Quit
            _                   -> pass
