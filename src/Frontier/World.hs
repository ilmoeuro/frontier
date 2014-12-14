{-# LANGUAGE RecordWildCards #-}
module Frontier.World
    (World(..)
    ) where
-- TODO: more efficient

import Control.Lens
import Control.Monad
import Data.Maybe
import Frontier.Feature
import Frontier.Feature.Qualifier
import Frontier.Features

data World = World
    {width :: Int
    ,height :: Int
    ,cells :: [((Int, Int), Thing Object)]
    }

symbol' :: Thing Object -> Char
symbol' obj =
    case catMaybes chars of
        c:_  -> c
        _    -> '?'
    where
        chars = withFeatures $ \Feature{..} pr ->
            symbol `fmap` (obj ^? pr)

instance Show World where
    show World{..} = do
        j <- [0..height]
        mplus
            (do
                i <- [0..width]
                return
                    . maybe ' ' symbol'
                    $ lookup (j,i) cells)
            "\n"
