module Frontier.Feature
    (Feature(..)
    ) where

import Frontier.Feature.Action (ActionM)

data Feature item object = Feature
    {initialItems           :: [item]
    ,symbol                 :: object -> Char
    ,action                 :: Char -> ActionM item object ()
    ,initPlayerCharacter    :: object
    }
