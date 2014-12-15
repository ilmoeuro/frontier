{-# LANGUAGE RankNTypes #-}
module Frontier.Feature
    (Feature(..)
    ) where

import Frontier.Feature.Action (ActionM)
import Frontier.Feature.Qualifier

data Feature a = Feature
    {initItems              :: [a Item]
    ,symbol                 :: a Object -> Char
    ,command                :: Char -> ActionM a ()
    ,initPlayerCharacter    :: a Object
    ,eq                     :: forall b. a b -> a b -> Bool
    }
