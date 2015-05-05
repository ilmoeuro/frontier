{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}
module Frontier.Model.Core
    (Object
    ,ModelM
    ,ModelState()
    ,mkModelState
    ,lastMessage
    ,allObjects
    ,changedObjects
    ,init
    ,command
    ,step
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad.State.Strict
import Data.Function
import Data.List hiding (init)
import qualified Frontier.Model.Core.Dynamic as Dyn
import Frontier.Model.Core.Static
import Prelude hiding (init)

type Object = ((Int, Int), Char)

newtype ModelState = ModelState { unModelState :: World }

type ModelM = State ModelState

mkModelState :: ModelState
mkModelState = ModelState mkWorld

runAction :: (World -> World) -> ModelM ()
runAction f = modify (ModelState . f . unModelState)

lastMessage :: ModelM String
lastMessage = (\x -> if any ('\n' `elem`) x
                then intercalate "" x
                else intercalate "; " x)
            .   messages
            .   unModelState
            <$> get

allObjects :: ModelM [Object]
allObjects = map toSprite
           . sortBy (compare `on` (^. _meta . __zIndex))
           . objects
           . unModelState
           <$> get
    where toSprite =
                 (,)
                 <$> view (_meta . __position)
                 <*> view (_meta . __symbol)

changedObjects :: ModelM [Object]
changedObjects = do
        result <- (++) <$> blanks <*> sprites
        runAction (_dirtyTiles .~ [])
        return result
    where
        blanks   = map (,' ')
                 . dirtyTiles
                 . unModelState
                 <$> get
        sprites  = map toSprite
                 . sortBy (compare `on` (^. _meta . __zIndex))
                 . dirtyObjects
                 . unModelState
                 <$> get
        dirtyObjects World{dirtyTiles, objects}
                 = filter (view (_meta . __position . to (`elem` dirtyTiles)))
                   objects
        toSprite =
                 (,)
                 <$> view (_meta . __position)
                 <*> view (_meta . __symbol)

init :: ModelM ()
init = runAction Dyn.init

command :: String -> ModelM ()
command = runAction . Dyn.command

step :: ModelM ()
step = runAction Dyn.step
