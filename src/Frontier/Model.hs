{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Frontier.Model
    (Input(..)
    ,Output(..)
    ,ModelState()
    ,mkModelState
    ,model
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Frontier.Model.Dynamic
import Frontier.Model.Static
import Pipes
import Prelude hiding (init)

type ModelM = Pipe Input Output (State ModelState)

data Input
    = Init
    | Command Char
    | Step

data Output
    = Display (Map (Int,Int) Char)

newtype ModelState = ModelState { unModelState :: World }

mkModelState :: ModelState
mkModelState = ModelState mkWorld

runAction :: (World -> World) -> ModelM ()
runAction f = modify (ModelState . f . unModelState)

display :: ModelM ()
display = sprites >>= yield . Display
    where
        sprites =
                ( Map.fromList
                . map toSprite
                . objects
                . unModelState
                ) `liftM` get
        toSprite =
                (,)
                <$> view (_meta . __position)
                <*> view (_meta . __symbol)

model :: Pipe Input Output (State ModelState) ()
model = go
    where
        go = await >>= \case
            Init ->         runAction init          >> display  >> go
            Command c ->    runAction (command c)   >> display  >> go
            Step ->         runAction step          >> display  >> go
