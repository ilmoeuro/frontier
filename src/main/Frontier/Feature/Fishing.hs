{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE OverloadedStrings   #-}
module Frontier.Feature.Fishing
    (Component()
    ,fromTag
    ,feature
    ) where

import Prelude hiding (init, lex)
import Data.Monoid
import Data.Maybe
import Control.Applicative
import Control.Lens hiding (contains)
import Text.Regex.Applicative
import Frontier.Feature
import Frontier.Fro

data Component a where
    PlayerCharacter             :: Component Object
    Water                       :: Component Object
    Boat                        :: Component Object
    Unknown                     :: Component a

deriving instance Eq (Component a)

fromTag :: Tag b -> Component b
fromTag PlayerCharacterTag     =  PlayerCharacter
fromTag _                      =  Unknown

welcomeMessage :: [String]
welcomeMessage =
    [
    ]

feature :: forall e w. Env w e
        -> (forall b. ALens' (e b) (Component b))
        -> Feature w
feature env@Env{..} _com = Feature {..} where

    _com' :: Lens' (e b) (Component b)
    _com' = cloneLens _com

    init :: Action w
    init = message (++ [unlines welcomeMessage])

    command :: String -> Action w
    -- Help message
    command "?" = message (++ [unlines welcomeMessage])
    -- Moving (by sails)
    command c | c `elem` ["h", "j", "k", "l"] =
        withAll Object $ \objs -> mconcat . mconcat $
             [  [  modify Object move pc
                <> modify Object move boat
                |  pc <- objs
                ,  boat <- objs
                ,  water <- objs
                ,  isPC pc
                ,  isBoat boat
                ,  isWater water
                ,  move pc ^. _position == water ^. _position
                ]
            ,   [  modify Object move pc
                |  pc <- objs
                ,  boat <- objs
                ,  isPC pc
                ,  isBoat boat
                ,  move pc ^. _position == boat ^. _position
                ]
            ]
      where
        isPC e | PlayerCharacter <- e ^# _com = True
        isPC _                                = False
        isWater e | Water <- e ^# _com        = True
        isWater _                             = False
        isBoat e | Boat <- e ^# _com          = True
        isBoat _                              = False
        move = case c of
            (x:_)   -> moveToDir env x
            _       -> id
    command _ = mempty

    step :: Action w
    step = mempty

    loadLevel :: LevelSource -> Action w
    loadLevel = fromMaybe mempty . match file . lex where
        file =  
                mconcat
            <$> many item
        item = 
                make
                    OpaqueTag
                    '~' 
                    ( (_com' .~ Water)
                    . (_zIndex .~ -1000)
                    )
                    <$  sym "Water"
                    <*> range
                    <*> range
                    <*  sym ";"
            <|> make
                    OpaqueTag
                    '_'
                    ( (_com' .~ Boat)
                    . (_zIndex .~ -500)
                    )
                    <$  sym "Boat"
                    <*> range
                    <*> range
                    <*  sym ";"
            <|> unknown
        make tag sy f (x1,x2) (y1,y2) =
            mconcat
                [create 
                    Object
                    tag 
                    ( (_position .~ (x,y)) 
                    . (_symbol .~ sy)
                    . f)
                | x <- [x1..x2]
                , y <- [y1..y2]
                ]
