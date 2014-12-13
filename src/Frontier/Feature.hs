module Frontier.Feature
    (Feature(..)
    ) where

import Frontier.Feature.Action (ActionM)
import Frontier.Feature.Qualifier

data Feature a = Feature
    {initItems              :: [a Item]
    ,symbol                 :: a Object -> Char
    ,action                 :: Char -> ActionM a ()
    ,initPlayerCharacter    :: a Object
    }
