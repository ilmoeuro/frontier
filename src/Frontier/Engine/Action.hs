{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontier.Engine.Action
    (Context(..)
    ,actionEnabled
    ,performAction
    ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Free
import Data.Maybe
import Control.Lens hiding (Context)
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier
import qualified Frontier.Engine.Monad as M

(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(.:) = (.).(.)

data Context a = Context
    {neighbors          :: [(Direction, a Object)]
    ,inventory          :: [a Item]
    ,this               :: a Object
    }

actionEnabled :: Context a -> Feature a -> ActionM a b -> Bool
actionEnabled
  ctx@Context{neighbors,inventory,this}
  f@Feature{eq}
    = isJust .: iterT $ \case
        (ShortDescription _ next)           -> next
        (Target (InventoryItem fn) next)   -> do
            let isEnabled i = actionEnabled ctx f (fn i)
            guard . or $ map isEnabled inventory
            next
        (Target (NearObject fn) next)       -> do
            let isEnabled o = actionEnabled ctx f (fn o)
            guard . or $ map (isEnabled.snd) neighbors
            next
        (Target (EmptySpace fn) next)       -> do
            guard $ length neighbors /= 8
            guard $ actionEnabled ctx f fn
            next
        (UseItem _ itm next)                -> do
            guard $ any (itm `eq`) inventory
            next
        (YieldItem _ next)                  -> next
        (Me next)                           -> next this
        (Move dir next)                     -> do
            guard . isNothing $ lookup dir neighbors
            next

performAction :: (MonadState (M.EngineState a) (t1 Maybe), 
                  MonadTrans t1)
              => Feature a 
              -> ActionM a b 
              -> t1 Maybe b 
performAction
    Feature{}
    = iterTM $ \case -- TODO: actually do something
        (ShortDescription _ next)           ->
            next
        (Target _ next)                     ->
            next
        (UseItem _ _ next)                  ->
            next
        (YieldItem _ next)                  ->
            next
        (Me next)                           ->
            gets M.playerCharacter >>= next.snd
        (Move dir next)                     -> do
            case dir of 
                N   -> M._playerCharacter._1._2 -= 1
                E   -> M._playerCharacter._1._1 += 1
                S   -> M._playerCharacter._1._2 += 1
                W   -> M._playerCharacter._1._1 -= 1
                -- TODO: other directions
                _   -> return ()
            next