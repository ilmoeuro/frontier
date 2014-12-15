{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
module Frontier.Engine.Action
    (actionEnabled
    ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.Free
import Data.Maybe
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier
import Frontier.Features

data ActionCtx = ActionCtx
    {neighbors          :: [(Direction, Thing Object)]
    ,inventory          :: [Thing Item]
    ,this               :: [Thing Object]
    }

actionEnabled :: (forall a. ActionM a ()) -> ActionCtx -> Bool
actionEnabled action ActionCtx{..} =
    any isJust
    $ withFeatures
    $ \Feature{..} pr ->
        let extract = mapMaybe (^? pr)
            inventory' = extract inventory
            this' = listToMaybe (extract this)
            -- TODO: lens-ify (?) (type modification misses Field1 instance)
            neighbors' = neighbors & each %~ (\(a,b) -> (a,b ^? pr))
        in
        flip iterT action $ \case
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
