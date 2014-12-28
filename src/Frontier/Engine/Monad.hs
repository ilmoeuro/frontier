module Frontier.Engine.Monad
    (EngineT
    ,pickItem
    ,pickObject
    ,pickEmptySpace
    ) where

import Control.Monad.State
import Frontier.Engine.World
import Frontier.Feature.Qualifier
import Pipes

data Input
    = KeyPress Char
data Output
    = Message String

type EngineT a base =
    Pipe
        Input
        Output
        (StateT (WorldState a) base)

-- TODO: safe
pickItem :: Monad m => EngineT a m (a Item)
pickItem = do
    items <- gets inventory
    let itemList = zip [1..] items
        numbers :: [(Char, Integer)]
        numbers = zip ['0'..'9'] [0..9]
    yield $ Message "Press a number key"
    KeyPress c <- await
    let Just num = lookup c numbers
        Just item = lookup num itemList
    return item

pickObject :: Monad m => EngineT a m ((Int, Int), a Object)
pickObject = undefined

pickEmptySpace :: Monad m => EngineT a m (Int, Int)
pickEmptySpace = undefined
