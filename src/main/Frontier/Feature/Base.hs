{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}
module Frontier.Feature.Base
    (Component()
    ,fromTag
    ,feature
    ) where

import Prelude hiding (init, lex)
import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.List hiding (init)
import Control.Lens
import Text.Regex.Applicative
import Frontier.Feature
import Frontier.Prelude
import Frontier.Fro

data Component a where
    PlayerCharacter             :: Component Object
    WorldItem                   :: Tag Item -> Component Object
    Unknown                     :: Component a

deriving instance (Show (Component a))

_PlayerCharacter :: Prism' (Component Object) ()
_PlayerCharacter = prism'
    (const PlayerCharacter)
    (\case
        PlayerCharacter -> Just ()
        _               -> Nothing)

_WorldItem :: Prism' (Component Object) (Tag Item)
_WorldItem = prism'
    WorldItem
    (\case
        WorldItem x     -> Just x
        _               -> Nothing)

_Unknown :: Prism' (Component Object) ()
_Unknown = prism'
    (const Unknown)
    (\case
        Unknown         -> Just ()
        _               -> Nothing)

fromTag :: Tag b -> Component b

fromTag PlayerCharacterTag      =  PlayerCharacter
fromTag (WorldItemTag x)        =  WorldItem x
fromTag _                       =  Unknown

name :: Tag b -> String
name PlayerCharacterTag         = "Player character"
name WallTag                    = "Wall"
name HammerTag                  = "Hammer"
name AxeTag                     = "Axe"
name LumberTag                  = "Lumber"
name (WorldItemTag x)           = "Item: " ++ name x
name OpaqueTag                  = "???"

welcomeMessage :: [String]
welcomeMessage =
    ["Welcome to Frontier!"
    ,""
    ,"Symbols"
    ,"  @ - you"
    ,"  ^ - tree"
    ,"  # - wall"
    ,""
    ,"Commands"
    ,"  q   - quit"
    ,"  ?   - show this help message"
    ,"  h/← - move left"
    ,"  j/↓ - move down"
    ,"  k/↑ - move up"
    ,"  l/→ - move right"
    ,"  p   - pickup"
    ,"  i   - show inventory"
    ,"  r   - repeat last command"
    ,"  0-9 - repeat next command multiple times"
    ]

feature :: forall e w. Env w e
        -> (forall b. ALens' (e b) (Component b))
        -> Feature w
feature env@Env{..} _com = Feature {..} where

    init :: Action w
    init = message (++ [unlines welcomeMessage])

    command :: String -> Action w
    -- Help screen
    command "?" = message (++ [unlines welcomeMessage])
    -- Moving
    command c | c `elem` ["h", "j", "k", "l"] =
        withAll Object $ \objs -> mconcat
            [ modify Object move pc
            | pc <- objs
            , (pc ^# _com) `matches` _PlayerCharacter
            , noCollision env (move pc) (filter (not . isWorldItem) objs)
            ]
      where
        isWorldItem e | WorldItem _ <- e ^# _com = True
        isWorldItem _                            = False
        move = case c of
            (x:_)   -> moveToDir env x
            _       -> id
    -- Show inventory
    command "i" =
        withAll Item $ \items ->
            message
                . flip (++)
                . (:[])
                . ("Inventory:\n\n" ++)
                . intercalate "\n"
                . zipWith annotate itemHandles
                . combineEquals
                . sort
                . map (name . (^. _tag))
                $ items
            where
              annotate handle (item, count)
                | count == 1
                    = handle : " - " ++ item
                | otherwise
                    = handle : " - " ++ item ++ " (x" ++ show count ++ ")"
    -- Object pickup
    command "p" =
        withAll Object $ \objs -> mconcat
            [  destroy Object obj
            <> create Item (getItemTag obj) id
            <> message (++ ["Picked up " ++ (name . getItemTag) obj ++ "."])
            |  obj <- objs
            ,  pc <- objs
            ,  (obj ^# _com) `matches` _WorldItem
            ,  (pc ^# _com) `matches` _PlayerCharacter
            ,  pc^._position == obj^._position
            ]
      where
        getItemTag obj | WorldItem s <- obj ^# _com = s
        getItemTag _ = error "Base.hs: Trying to get itemTag of non-WorldItem"
    command _ = mempty

    step :: Action w
    step =
        -- Clear previous messages
        message (const [])

    loadLevel :: LevelSource -> Action w
    loadLevel = fromMaybe mempty . match file . lex where
        file =  
                mconcat
            <$> many item
        item = 
                object 
                    "Wall" 
                    OpaqueTag 
                    '#'
                    id
            <|> object
                    "PlayerCharacter"
                    PlayerCharacterTag 
                    '@'
                    (_zIndex .~ 1000)
            <|> unknown
        object identifier tag sy f =
                make tag sy f
                    <$  sym identifier
                    <*> range
                    <*> range
                    <*  sym ";"
        make' tag sy f x y =
                create 
                    Object
                    tag 
                    ( (_position .~ (x,y)) 
                    . (_symbol .~ sy)
                    . f)
        make tag sy f (x1,x2) (y1,y2) =
                mconcat [ make' tag sy f x y
                        | y <- [y1..y2]
                        , x <- [x1..x2]
                        ]
