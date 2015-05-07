{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Core.Feature.Prelude
    (noCollision
    ,moveToDir
    ) where

import Control.Lens
import Frontier.Core.Feature

moveToDir :: Env w e -> Char -> e Object -> e Object
moveToDir Env{..} = \case
            'h' -> _position._1 -~ 1
            'j' -> _position._2 +~ 1
            'k' -> _position._2 -~ 1
            'l' -> _position._1 +~ 1
            _   -> id

noCollision :: Env w e -> e Object -> [e Object] -> Bool
noCollision Env{..} obj objs =
        and
            [ position obj /= position obj'
            | obj' <- objs
            , not (obj `is` obj')
            ]
    where
        position = view _position
