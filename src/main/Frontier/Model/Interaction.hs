{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Frontier.Model.Interaction
    (Sprite
    ,Input(..)
    ,Output(..)
    ,ModelState()
    ,_Display
    ,_Message
    ,mkModelState
    ,model
    ) where

import Control.Lens
import Control.Monad.State.Strict
import Data.Char
import Data.List
import Data.List.Split
import qualified Frontier.Model.Core as Core
import Pipes
import Prelude hiding (init)

type Sprite = ((Int, Int), Char)

data Input
    = KeyChar Char

data Output
    = Display String [Sprite]
    | Message [String]

makePrisms ''Output

data ModelState = ModelState
    { coreState :: Core.ModelState
    , count     :: Int
    }
makeLensesFor
    [("coreState"   ,"_coreState")
    ,("count"       ,"_count")
    ]
    ''ModelState

type ModelM = Pipe Input Output (State ModelState)

data Token = TokenCmd String | TokenCount Int

welcomeMessage :: [String]
welcomeMessage =
    ["Welcome to Frontier!"
    ,""
    ,"Commands:"
    ,"h - move left"
    ,"j - move down"
    ,"k - move up"
    ,"l - move right"
    ,"B - build (takes a direction as argument)"
    ,"P - pickup (takes a direction as argument)"
    ,"i - show inventory"
    ,""
    ,"Symbols:"
    ,"@ - you"
    ,"^ - tree"
    ,"# - wall"
    ,""
    ,"Press any key to start"
    ]

runCore :: Core.Input -> ModelM ()
runCore input =
    lift (zoom _coreState (Core.model input)) >>= showResult
  where
    showResult (Core.Display msg sprites)
        | '\n' `elem` msg = do
            let msgs = splitOn "; " msg
            forM_ msgs $ \msg' -> do
                yield . Message $ splitOn "\n" msg'
                void await
            yield (Display "" sprites)
        | otherwise       = yield (Display msg sprites)

mkModelState :: ModelState
mkModelState = ModelState Core.mkModelState 0

getToken :: ModelM Token
getToken
    = await >>= \(KeyChar c)    ->
            if | nullary c      -> return . TokenCmd $ [c]
               | unary c        -> await >>= \(KeyChar d) ->
                                   return . TokenCmd $ [c,d]
               | binary c       -> await >>= \(KeyChar d) ->
                                   await >>= \(KeyChar e) ->
                                   return . TokenCmd $ [c,d,e]
               | isDigit c      -> return . TokenCount . read $ [c]
               | otherwise      -> return . TokenCmd $ ""
  where
    nullary x       = x `elem` ['a'..'z']
    unary x         = x `elem` ['A'..'Z']
    binary x        = x `elem` "+!\"#%&/()=?@${[]}\\'*<,.->;:_|"

model :: ModelM ()
model = do
    yield (Message welcomeMessage)
    void await
    runCore Core.Init
    go
  where
    go = getToken >>= \case
            (TokenCmd c)        -> unless (c == "q") $ do
                                        n <- gets (max 1 . count)
                                        replicateM_ n $ do
                                            runCore Core.Step
                                            runCore (Core.Command c)
                                        _count .= 0
                                        go
            (TokenCount n)      -> do
                                        _count *= 10
                                        _count += n
                                        go

_unused1 :: ModelState -> Core.ModelState
_unused1 = coreState
