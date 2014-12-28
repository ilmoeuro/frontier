{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontier.Engine.Action
    (actionEnabled
    ,performAction
    ) where

import Control.Lens hiding (Action, Context)
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Frontier.Engine.World
import Frontier.Engine.Monad
import Frontier.Feature
import Frontier.Feature.Action

pass :: (MonadTrans t, Monad m, Monad (t m))
     => ActionF a (t m b)
     -> t m b
pass = \case
    (ShortDescription _ next)           ->
        next
    (TargetItem next)                   ->
        next $ error "Frontier.Engine.Action.pass"
    (TargetObject next)                 ->
        next $ error "Frontier.Engine.Action.pass"
    (TargetEmptySpace next)             ->
        next
    (ModifyTargetItem _ next)           ->
        next
    (ReplaceTargetItem _ next)          ->
        next
    (DestroyTargetItem next)            ->
        next
    (ModifyTargetObject _ next)         ->
        next
    (ReplaceTargetObject _ next)        ->
        next
    (DestroyTargetObject next)          ->
        next
    (UseItem _ _ next)                  ->
        next
    (YieldItem _ next)                  ->
        next
    (Me next)                           ->
        next $ error "Frontier.Engine.Action.pass"
    (Move _ next)                       ->
        next

actionEnabled :: (MonadState (WorldState a) m
                 ,Monad m
                 )
              => Feature a
              -> ActionT a (EngineT a m) b
              -> EngineT a m (Maybe b)
actionEnabled
  Feature{eq}
    = runActionT $ \case
        (ShortDescription _ next)           -> next
        (TargetItem next)                   ->
            lift (gets inventory) >>= msum . map next
        (TargetObject next)                 ->
            lift (gets neighbors) >>= msum . map (next.snd)
        (TargetEmptySpace next)             -> do
            lift (gets neighbors) >>= guard . (/= 8) . length
            next
        (UseItem _ itm next)                -> do
            lift (gets inventory) >>= guard . any (itm `eq`)
            next
        (Me next)                           ->
            lift (gets playerCharacter) >>= next . snd
        (Move dir next)                     -> do
            lift (gets neighbors) >>= guard . isNothing . lookup dir
            next
        others                              -> pass others

performAction :: Monad m
              => Feature a
              -> ActionT a (EngineT a m) b
              -> EngineT a m (Maybe b)
performAction
    Feature{}
    = runActionT $ \case
        (TargetItem next)                   -> do
            item <- lift pickItem
            lift $ _targetItem .= item
            next item
        (TargetObject next)                 -> do
            obj@(_, obj') <- lift pickObject
            lift $ _targetObject .= obj
            next obj'
        (TargetEmptySpace next)             -> do
            lift $ pickEmptySpace >>= (_targetEmptySpace .=)
            next
        (Me next)                           ->
            lift (gets playerCharacter) >>= next . snd
        (Move dir next)                     -> do
            lift $ case dir of
                N   -> _playerCharacter._1._2 -= 1
                E   -> _playerCharacter._1._1 += 1
                S   -> _playerCharacter._1._2 += 1
                W   -> _playerCharacter._1._1 -= 1
                _   -> return ()
            next
        others                              -> pass others
