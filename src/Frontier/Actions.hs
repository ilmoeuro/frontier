{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TupleSections  #-}
module Frontier.Actions
    (ActionM
    ,move
    ,chop
    ,build
    ) where

import Control.Applicative
import Control.Conditional (guardM)
import Control.Lens
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Frontier.Data
import Frontier.Monad

type ActionM = MaybeT FrontierM

which :: (a -> Bool) -> Traversal' a a
which p f a | p a  = f a
which _ _ a        = pure a

neighbor' :: (Int, Int) -> Direction -> Lens' World (Maybe Object)
neighbor' (px,py) = \case
    N -> _objects . at (px,py-1)
    E -> _objects . at (px+1,py)
    S -> _objects . at (px,py+1)
    W -> _objects . at (px-1,py)

neighbor :: Direction -> Lens' World (Maybe Object)
neighbor dir = lens
    (\w@World{playerCharacter} ->
        let (p,_) = playerCharacter in w ^. neighbor' p dir)
    (\w@World{playerCharacter} x ->
        let (p,_) = playerCharacter in w & neighbor' p dir .~ x)

move :: Direction -> ActionM ()
move = \case
    N -> y -= 1
    E -> x += 1
    S -> y += 1
    W -> x -= 1
  where
    x = _playerCharacter._1._1
    y = _playerCharacter._1._2

chop :: Direction -> ActionM ()
chop dir = do
    guardM . gets $ elem Axe . items
    guardM . use $ neighbor dir . to (== Just Tree)
    neighbor dir .= Nothing
    _items %= (Lumber :)

build :: Direction -> ActionM ()
build dir = do
    guardM . gets $ elem Lumber . items
    guardM . gets $ elem Hammer . items
    neighbor dir .= Just Wall
