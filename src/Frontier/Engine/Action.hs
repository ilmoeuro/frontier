{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Engine.Action
    (ActionEnabledParams(..)
    ,actionEnabled
    ) where

import Control.Monad
import Control.Monad.Trans.Free
import Data.Maybe
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier

(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(.:) = (.).(.)

data ActionEnabledParams a = ActionEnabledParams
    {neighbors          :: [(Direction, a Object)]
    ,inventory          :: [a Item]
    ,this               :: a Object
    }

actionEnabled :: ActionEnabledParams a -> Feature a -> ActionM a b -> Bool
actionEnabled param@ActionEnabledParams{..} f = isJust .: iterT $ \case
    (ShortDescription _ next)           -> next
    (Target (InventoryItem fn) next)   -> do
        let isEnabled i = actionEnabled param f (fn i)
        guard . or $ map isEnabled inventory
        next
    (Target (NearObject fn) next)       -> do
        let isEnabled o = actionEnabled param f (fn o)
        guard . or $ map (isEnabled.snd) neighbors
        next
    (Target (EmptySpace fn) next)       -> do
        guard $ length neighbors /= 8
        guard $ actionEnabled param f fn
        next
    (UseItem _ itm next)                -> do
        guard $ any (eq f itm) inventory
        next
    (YieldItem _ next)                  -> next
    (Me next)                           -> next this
    (Move dir obj next)                 -> do
        guard $ eq f obj this
        guard . isNothing $ lookup dir neighbors
        next
