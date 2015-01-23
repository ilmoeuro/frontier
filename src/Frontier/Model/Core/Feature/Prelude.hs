{-# LANGUAGE RecordWildCards #-}
module Frontier.Model.Core.Feature.Prelude
    (when
    ,noCollision
    ) where

import Control.Lens
import Frontier.Model.Core.Feature

when :: Bool -> (w -> w) -> w -> w
when True  x = x
when False _ = id

noCollision :: Env w e -> e Object -> [e Object] -> Bool
noCollision Env{..} obj objs =
        and
            [position obj /= position obj'
            | obj' <- objs
            , not (obj `is` obj')
            ]
    where
        position = view _position
