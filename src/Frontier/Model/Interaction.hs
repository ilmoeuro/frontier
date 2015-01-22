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
import qualified Frontier.Model.Core as Core
import Pipes
import Prelude hiding (init)

type Sprite = ((Int, Int), Char)

data Input
    = KeyChar Char

data Output
    = Display [Sprite]
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
    ,"b - build (takes a direction as argument)"
    ,""
    ,"Symbols:"
    ,"@ - you"
    ,"^ - tree"
    ,"# - wall"
    ,""
    ,"Press 's' to start"
    ]

runCore :: Core.Input -> ModelM ()
runCore input =
    lift (zoom _coreState (unpack `liftM` Core.model input)) >>= yield
  where
    unpack (Core.Display sprites) = Display sprites

mkModelState :: ModelState
mkModelState = ModelState Core.mkModelState 1

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
               | otherwise      -> return . TokenCmd $ [c]
  where
    nullary x       = x `elem` "hjklq'*!\"#¤%&/()=?@{[]}\\+<>,;.:-_"
    unary x         = not (nullary x) && x `elem` ['a'..'z']
    binary x        = x `elem` ['A'..'Z']

model :: ModelM ()
model = do
    yield (Message welcomeMessage)
    _ <- await
    runCore Core.Init
    go
  where
    go = runCore Core.Step >>
         getToken >>= \case
            (TokenCmd c)        -> unless (c == "q") $ do
                                        n <- gets (max 1 . count)
                                        replicateM_ n $
                                            runCore (Core.Command c)
                                        _count .= 0
                                        go
            (TokenCount n)      -> do
                                        _count *= 10
                                        _count += n
                                        go

_unused1 :: ModelState -> Core.ModelState
_unused1 = coreState
