module Frontier.Model.Core
    (Sprite
    ,Input(..)
    ,Output(..)
    ,ModelState()
    ,mkModelState
    ,model
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Frontier.Model.Core.Dynamic
import Frontier.Model.Core.Static
import Prelude hiding (init)

type Sprite = ((Int, Int), Char)

{--
    Nullary commands:   h, j, k, l, q, punctuation ("q" is never valid)
    Unary commands:     a-z (sans hjklq) and one character
    Binary commands:    A-Z and two characters
    Numbers are reserved for count
--}
data Input
    = Init
    | Command String
    | Step
    deriving (Show)

data Output
    = Display [Sprite]
    deriving (Show)

newtype ModelState = ModelState { unModelState :: World }

type ModelM = State ModelState

mkModelState :: ModelState
mkModelState = ModelState mkWorld

runAction :: (World -> World) -> ModelM ()
runAction f = modify (ModelState . f . unModelState)

display :: ModelM Output
display = Display `liftM` sprites
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

model :: Input -> ModelM Output
model Init              = runAction init        >> display
model (Command s)       = runAction (command s) >> display
model Step              = runAction step        >> display
