{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TemplateHaskell  #-}
module Frontier.Model.Interaction
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
    = DisplayDelta String [Sprite]
    | DisplayFull String [Sprite]
    | Message [String]

makePrisms ''Output

data ModelState = ModelState
    { coreState :: Core.ModelState
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

welcomeMessage :: [String]
welcomeMessage =
    ["Welcome to Frontier!"
    ,""
    ,"Some basic commands:"
    ,"h - move left"
    ,"j - move down"
    ,"k - move up"
    ,"l - move right"
    ,"C - chop (takes a direction as argument)"
    ,"B - build (takes a direction as argument)"
    ,"p - pickup"
    ,"i - show inventory"
    ,"r - repeat last command"
    ,"q - quit"
    ,""
    ,"Arrow keys are aliases for `hjkl`"
    ,"Esc is an alias for `q`"
    ,""
    ,"You can prefix any command with a count."
    ,""
    ,"Symbols:"
    ,"@ - you"
    ,"^ - tree"
    ,"# - wall"
    ,""
    ,"Press any key to start"
    ]

mkModelState :: ModelState
mkModelState = ModelState Core.mkModelState 0 ""

runCore :: Core.ModelM a -> ModelM a
runCore = lift . zoom _coreState

displayOutput :: ModelM ()
displayOutput = do
    msg <- runCore Core.lastMessage
    objs <- runCore Core.changedObjects
    case splitOn "\n" msg of
        []      -> yield (DisplayDelta "" objs)
        [_]     -> yield (DisplayDelta msg objs)
        msgs    -> do
            yield (Message msgs)
            void await
            runCore Core.allObjects >>= yield . DisplayFull ""

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
    runCore Core.init
    runCore Core.allObjects >>= yield . DisplayFull ""
    go
  where
    runToken = \case
        (TokenCmd "q")      -> return ()
        (TokenCmd "r")      -> use _lastCmd >>= runToken . TokenCmd
        (TokenCmd c)        -> do
                                    n <- gets (max 1 . count)
                                    replicateM_ n $ do
                                        runCore Core.step
                                        runCore $ Core.command c
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

_unused1 :: ModelState -> Core.ModelState
_unused1 = coreState

_unused2 :: ModelState -> String
_unused2 = lastCmd
