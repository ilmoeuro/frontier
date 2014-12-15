{-# LANGUAGE RecordWildCards #-}
module Frontier.World
    (World(..)
    ) where
-- TODO: more efficient

import Control.Monad
import Frontier.Feature
import Frontier.Feature.Qualifier
import Frontier.Features

data World = World
    {width :: Int
    ,height :: Int
    ,cells :: [((Int, Int), Thing Object)]
    }

instance Show World where
    show World{..} = do
        j <- [0..height]
        mplus
            (do
                i <- [0..width]
                return
                    . maybe ' ' (contravariant symbol)
                    $ lookup (j,i) cells)
            "\n"
