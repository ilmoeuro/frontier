{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Frontier.Model.Core
    (Sprite
    ,Input(..)
    ,Output(..)
    ,ModelState()
    ,_Display
    ,mkModelState
    ,model
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Frontier.Model.Core.Dynamic
import Frontier.Model.Core.Static
import Pipes
import Prelude hiding (init)

type Sprite = ((Int, Int), Char)

data Input
    = Init
    -- TODO: More refined commands
    | Command Char
    | Step
    deriving (Show)

data Output
    = Display [Sprite]
    deriving (Show)

makePrisms ''Output

newtype ModelState = ModelState { unModelState :: World }

type ModelM = Pipe Input Output (State ModelState)

mkModelState :: ModelState
mkModelState = ModelState mkWorld

runAction :: (World -> World) -> ModelM ()
runAction f = modify (ModelState . f . unModelState)

display :: ModelM ()
display = sprites >>= yield . Display
    where
        sprites =
                ( map toSprite
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
            Command 'q' ->  return ()
            Command c ->    runAction (command c)   >> display  >> go
            Step ->         runAction step          >> display  >> go
