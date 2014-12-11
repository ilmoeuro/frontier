{-# LANGUAGE EmptyDataDecls #-}
module Frontier.Feature
    (Feature(..)
    ) where
    
import Frontier.Feature.Action (ActionM)

data Feature object item = Feature
    {initialItems           :: [item]
    ,symbol                 :: object -> Char
    ,use                    :: item -> ActionM item object ()
    }