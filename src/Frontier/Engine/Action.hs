{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontier.Engine.Action
    (Context(..)
    ,actionEnabled
    ,performAction
    ) where

import Control.Lens hiding (Context, Action)
import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Frontier.Engine.Monad as M
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Qualifier

data Context a = Context
    {neighbors          :: [(Direction, a Object)]
    ,inventory          :: [a Item]
    ,this               :: a Object
    }

actionEnabled :: forall a b.
                 Context a
              -> Feature a
              -> ActionT a Identity b
              -> Bool
actionEnabled
  Context{neighbors,inventory,this}
  Feature{eq}
  action
    = isJust $ runIdentity $ runActionT go action where
        go :: (MonadTrans t, Monad (t Identity), MonadPlus (t Identity))
           => ActionF a (t Identity b)
           -> t Identity b
        go = \case
            (ShortDescription _ next)           -> next
            (TargetItem next)                   ->
                msum $ map next inventory
            (TargetObject next)                 ->
                msum $ map (next.snd) neighbors
            (TargetEmptySpace next)             -> do
                guard $ length neighbors /= 8
                next
            (UseItem _ itm next)                -> do
                guard $ any (itm `eq`) inventory
                next
            (YieldItem _ next)                  -> next
            (Me next)                           -> next this
            (Move dir next)                     -> do
                guard . isNothing $ lookup dir neighbors
                next

performAction :: MonadState (M.EngineState a) m 
              => Feature a 
              -> ActionT a m b
              -> m (Maybe b)
performAction
    Feature{}
    = runActionT $ \case -- TODO: actually do something
        (ShortDescription _ next)           ->
            next
        (TargetItem next)                   ->
            undefined >>=
            next
        (TargetObject next)                 ->
            undefined >>=
            next
        (TargetEmptySpace next)             ->
            next
        (UseItem _ _ next)                  ->
            next
        (YieldItem _ next)                  ->
            next
        (Me next)                           -> do
            playerCharacter <- lift $ gets M.playerCharacter
            next (snd playerCharacter)
        (Move dir next)                     -> do
            lift $ case dir of
                N   -> M._playerCharacter._1._2 -= 1
                E   -> M._playerCharacter._1._1 += 1
                S   -> M._playerCharacter._1._2 += 1
                W   -> M._playerCharacter._1._1 -= 1
                -- TODO: other directions
                _   -> return ()
            next
