{-# LANGUAGE EmptyDataDecls  #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Features.Farming
    (Object()
    ,Item()
    ,feature
    ) where

-- import Control.Monad
import Frontier.Feature
import Frontier.Feature.Action

data Object
    = PlayerCharacter

data Item

feature :: Feature Item Object
feature = Feature{..} where

    action :: Char -> ActionM Item Object ()
    action = const disableAction

    initialItems :: [Item]
    initialItems = []

    symbol :: Object -> Char
    symbol _ = '?'

    initPlayerCharacter :: Object
    initPlayerCharacter = PlayerCharacter
