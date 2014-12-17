{-# LANGUAGE RankNTypes #-}
module Frontier.Feature
    (Feature(..)
    ) where

import Frontier.Feature.Action (ActionM)
import Frontier.Feature.Qualifier

-- Maybe composable someday?
data Feature a = Feature
    {initItems              :: [a Item]
    ,symbol                 :: a Object -> Char
    ,command                :: Char -> Maybe (a (Action ()))
    ,initPlayerCharacter    :: a Object
    -- TODO: way to compare non-enum things
    ,eq                     :: forall b. a b -> a b -> Bool
    ,run                    :: forall b. a (Action b) -> ActionM a b
    }
