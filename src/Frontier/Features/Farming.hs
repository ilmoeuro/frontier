{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE EmptyDataDecls #-}
module Frontier.Features.Farming
    (Object()
    ,Item()
    ,feature
    ) where
    
-- import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action

data Object

data Item

feature :: Feature Object Item
feature = Feature{..} where

    use :: Item -> ActionM Item Object ()
    use _ = disableAction

    initialItems :: [Item]
    initialItems = []

    symbol :: Object -> Char
    symbol _ = ' '