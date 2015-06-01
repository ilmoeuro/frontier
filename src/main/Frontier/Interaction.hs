{-# OPTIONS_GHC -fno-warn-missing-signatures    #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE RecordWildCards  #-}
module Frontier.Interaction
    (Sprite
    ,Input(..)
    ,Output(..)
    ,ModelState()
    ,_DisplayDelta
    ,_DisplayFull
    ,_Message
    ,mkModelState
    ,model
    ) where

import Prelude hiding (init)
import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Data.Char
import Data.List.Split
import Pipes
import Data.Text.Encoding
import Data.FileEmbed
import qualified Frontier.Engine as Engine

Engine.Engine{..} = Engine.engine

type Sprite = ((Int, Int), Char)

data Input
    = KeyChar Char

data Output
    = DisplayDelta String [Sprite]
    | DisplayFull String [Sprite]
    | Message [String]

makePrisms ''Output

data ModelState = ModelState
    { coreState :: Engine.World
    , count     :: Int
    , lastCmd   :: String
    }
makeLensesFor
    [("coreState"   ,"_coreState")
    ,("count"       ,"_count")
    ,("lastCmd"     ,"_lastCmd")
    ]
    ''ModelState

type ModelM = Pipe Input Output (State ModelState)

data Token = TokenCmd String | TokenCount Int

mkModelState :: ModelState
mkModelState = ModelState Engine.mkWorld 0 ""

runEngine :: Engine.EngineM a -> ModelM a
runEngine = lift . zoom _coreState

displayLongMessage :: [String] -> ModelM ()
displayLongMessage msgs =
    forM_ (chunksOf 24 msgs) $ \msgs' -> do
        yield (Message msgs')
        void await

displayOutput :: ModelM ()
displayOutput = do
    msg <- runEngine lastMessage
    objs <- runEngine changedSprites
    case splitOn "\n" msg of
        []      -> yield (DisplayDelta "" objs)
        [_]     -> yield (DisplayDelta msg objs)
        msgs    -> do
            displayLongMessage msgs
            runEngine allSprites >>= yield . DisplayFull ""

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
    nullary x       = x `elem` ['a'..'z'] ++ "?"
    unary x         = x `elem` ['A'..'Z']
    binary x        = x `elem` "+!\"#%&/()=@${[]}\\'*<,.->;:_|"

model :: ModelM ()
model = do
    runEngine init
    runEngine . loadLevel . decodeUtf8 $ $(embedFile "src/data/level.fro")
    displayOutput
    go
  where
    runToken = \case
        (TokenCmd "q")      -> return ()
        (TokenCmd "r")      -> use _lastCmd >>= runToken . TokenCmd
        (TokenCmd c)        -> do
                                    n <- gets (max 1 . count)
                                    replicateM_ n $ do
                                        runEngine step
                                        runEngine $ command c
                                        displayOutput
                                    when (c `notElem` ["h", "j", "k", "l"])
                                        (_lastCmd .= c)
                                    _count .= 0
                                    go
        (TokenCount n)      -> do
                                    _count *= 10
                                    _count += n
                                    go
    go = getToken >>= runToken

_unused1 :: ModelState -> Engine.World
_unused1 = coreState

_unused2 :: ModelState -> String
_unused2 = lastCmd
