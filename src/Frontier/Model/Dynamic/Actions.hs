{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE FlexibleContexts   #-}
module Frontier.Model.Dynamic.Actions
    (move
    ,chop
    ,build
    ,smash
    ,query
    ,unbox
    ,step
    ) where

import Control.Conditional (guardM)
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.MultiSet as Ms
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
move dir = try $ do
  guardM . use $ _neighbor dir . to (== Nothing)
  case dir of
    N -> y -= 1
    E -> x += 1
    S -> y += 1
    W -> x -= 1
  where
    x = _playerCharacter._1._1
    y = _playerCharacter._1._2

chop :: MonadState World m => Direction -> m ()
chop dir = try $ do
    guardM . gets $ Ms.member Axe . items
    guardM . use $ _neighbor dir . to (== Just Tree)
    _neighbor dir .= Nothing
    _items %= Ms.insert Lumber

build :: MonadState World m => Direction -> m ()
build dir = try $ do
    guardM . gets $ Ms.member Lumber . items
    guardM . gets $ Ms.member Hammer . items
    _items %= Ms.delete Lumber
    _neighbor dir .= Just Wall

smash :: MonadState World m => Direction -> m ()
smash dir = try $ do
    guardM . gets $ Ms.member Hammer . items
    _neighbor dir .= Nothing
    
unbox :: MonadState World m => Direction -> m ()
unbox dir = use (_neighbor dir) >>= \case
    (Just (Box (Just item))) -> do
        _neighbor dir .= Just (Box Nothing)
        _items %= Ms.insert item
    _ -> return ()
    
query :: MonadState World m => Direction -> m String
query dir = use (_neighbor dir . to (maybe "" show))

step :: MonadState World m => m ()
step = _playerCharacter . _2 . _energy -= 1