{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Engine.Action
    (actionEnabled
    ,ActionCtx(..)
    ) where

import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Free
import Data.Maybe
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier
import Frontier.Features

-- Non-prime data & functions are run outside a feature
data ActionCtx = ActionCtx
    {neighbors          :: [(Direction, Generic Object)]
    ,inventory          :: [Generic Item]
    ,this               :: [Generic Object]
    }

-- Prime data & functions are run inside a feature
data ActionCtx' a = ActionCtx'
    {neighbors'         :: [(Direction, Maybe (a Object))]
    ,inventory'         :: [a Item]
    ,this'              :: Maybe (a Object)
    }

actionEnabled :: ActionCtx -> (forall a b. ActionM a b) -> Bool
actionEnabled ActionCtx{..} action =
    or $ withFeatures $ \ftr pr ->
        let extract = mapMaybe (^? pr)
            neighbors' = map (second (^? pr)) neighbors
            inventory' = extract inventory
            this' = listToMaybe (extract this)
        in actionEnabled' ftr ActionCtx'{..} action

actionEnabled' :: Feature a -> ActionCtx' a -> ActionM a b -> Bool
actionEnabled' Feature{..} ActionCtx'{..} action =
    isJust . flip iterT action $ \case
        (ShortDescription _ next) ->
            next
        (TargetInventoryItem next) ->
            msum $ map next inventory'
        (YieldInventoryItem _ next) ->
            next
        (ReplaceTargetItem _ next) ->
            next
        (DestroyTargetItem next) ->
            next
        (RequireItem _ item next) ->
            guard (any (`eq` item) inventory') >> next
        (Me next) ->
            maybe mzero next this'
        (TargetObject Near next) ->
            msum $ map next (mapMaybe snd neighbors')
        (TargetObject _ _) ->
            mzero
        (ReplaceTargetObject _ next) ->
            next
        (DestroyTargetObject next) ->
            next
        (Move dir obj next) -> do
            guard $ maybe False (obj `eq`) this'
            guard $ notElem dir $ map fst neighbors'
            next True
        (TargetEmptySpace next) ->
            guard (length neighbors' /= 8) >> next
