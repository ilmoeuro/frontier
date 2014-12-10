{-# LANGUAGE EmptyDataDecls #-}
module Frontier.Feature
    (Feature(..)
    ,ObjectTag
    ,ItemTag
    ) where
    
import Frontier.Feature.Action (ActionM)

data Feature object item = Feature
    {objectTag              :: object -> Maybe ObjectTag
    ,itemTag                :: item -> Maybe ItemTag
    ,tagObject              :: ObjectTag -> Maybe object
    ,tagItem                :: ItemTag -> Maybe item

    ,initialItems           :: [item]

    ,symbol                 :: object -> Char
    
    ,use                    :: item -> ActionM item object ()
    }
    
data ObjectTag
    
data ItemTag