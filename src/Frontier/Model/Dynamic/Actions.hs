{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleContexts   #-}
module Frontier.Model.Dynamic.Actions
    (move
    ,chop
    ,build
    ) where

import Control.Conditional (guardM)
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Maybe
import Frontier.Model.Static

_neighbor' :: (Int, Int) -> Direction -> Lens' World (Maybe Object)
_neighbor' (px,py) = \case
    N -> _objects . at (px,py-1)
    E -> _objects . at (px+1,py)
    S -> _objects . at (px,py+1)
    W -> _objects . at (px-1,py)

_neighbor :: Direction -> Lens' World (Maybe Object)
_neighbor dir = lens
    (\w@World{playerCharacter} ->
        let (p,_) = playerCharacter in w ^. _neighbor' p dir)
    (\w@World{playerCharacter} x ->
        let (p,_) = playerCharacter in w & _neighbor' p dir .~ x)

try :: Monad m => MaybeT m () -> m ()
try = (fromMaybe () `liftM`) . runMaybeT

move :: MonadState World m => Direction -> m ()
move = \case
    N -> y += 1
    E -> x += 1
    S -> y -= 1
    W -> x -= 1
  where
    x = _playerCharacter._1._1
    y = _playerCharacter._1._2

chop :: MonadState World m => Direction -> m ()
chop dir = try $ do
    guardM . gets $ elem Axe . items
    guardM . use $ _neighbor dir . to (== Just Tree)
    _neighbor dir .= Nothing
    _items %= (Lumber :)

build :: MonadState World m => Direction -> m ()
build dir = try $ do
    guardM . gets $ elem Lumber . items
    guardM . gets $ elem Hammer . items
    _neighbor dir .= Just Wall
