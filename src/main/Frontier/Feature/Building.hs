{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE OverloadedStrings   #-}
module Frontier.Feature.Building
    (Component()
    ,fromTag
    ,feature
    ) where

import Prelude hiding (init)
import Data.Monoid
import Data.Maybe
import Data.Char
import qualified Data.Text as Tx
import Control.Lens hiding (contains)
import Text.Regex.Applicative
import Frontier.Feature
import Frontier.Prelude

data Component a where
    PlayerCharacter             :: Component Object
    Tree                        :: Component Object
    Hammer                      :: Component Item
    Axe                         :: Component Item
    Lumber                      :: Component Item
    Unknown                     :: Component a

deriving instance Eq (Component a)

fromTag :: Tag b -> Component b
fromTag PlayerCharacterTag     =  PlayerCharacter
fromTag HammerTag              =  Hammer
fromTag AxeTag                 =  Axe
fromTag LumberTag              =  Lumber
fromTag _                      =  Unknown

welcomeMessage :: [String]
welcomeMessage =
    ["  C   - chop (takes a direction as argument)"
    ,"  B   - build (takes a direction as argument)"
    ]

feature :: forall e w. Env w e
        -> (forall b. ALens' (e b) (Component b))
        -> Feature w
feature env@Env{..} _com = Feature {..} where

    _com' :: Lens' (e b) (Component b)
    _com' = cloneLens _com

    init :: Action w
    init = mempty

    command :: String -> Action w
    -- Help message
    command "?" = message (++ [unlines welcomeMessage])
    -- Building
    command c | c `elem` ["Bh", "Bj", "Bk", "Bl"] = build where
        filterByComponent p = filter (\e -> p (e ^# _com))
        move = case c of
            (_:x:_)     -> moveToDir env x
            _           -> id
        createWall = withAll Object $ \objs -> mconcat
            [create Object OpaqueTag
                ( move
                . (_position   .~ (obj ^. _position))
                . (_symbol     .~ '#'))
            | obj <- objs
            , obj ^# _com == PlayerCharacter
            , noCollision env (move obj) objs
            ]
        build = withAll Item $ \items ->
            when' (not . null . filterByComponent (== Hammer) $ items)
            $ case filterByComponent (== Lumber) items of
                (lumber:_)  ->
                    destroy Item lumber
                    <> createWall
                _ -> mempty
    -- Chopping
    command c | c `elem` ["Ch", "Cj", "Ck", "Cl"] =
        withAll Object $ \objs -> mconcat
            [  destroy Object obj
            <> create Item LumberTag id
            |  obj <- objs
            ,  pc <- objs
            ,  obj ^# _com == Tree
            ,  pc ^# _com == PlayerCharacter
            ,  move pc^._position == obj^._position
            ]
      where
        move = case c of
            (_:x:_)     -> moveToDir env x
            _           -> id
    command _ = mempty

    step :: Action w
    step = mempty

    loadLevel :: LevelSource -> Action w
    loadLevel = fromMaybe mempty . match file . Tx.words where
        file =  
                mconcat
            <$> many item
        item = 
                make (WorldItemTag HammerTag) '/'
                    <$  sym "Hammer"
                    <*> number
                    <*> number
                    <*  sym ";"
            <|> make (WorldItemTag AxeTag) '/'
                    <$  sym "Axe"
                    <*> number
                    <*> number
                    <*  sym ";"
            <|> make' OpaqueTag '#' (_com' .~ Tree)
                    <$  sym "Tree"
                    <*> number
                    <*> number
                    <*  sym ";"
            <|> mempty 
                    <$ some (psym (/= ";"))
                    <* sym ";"
        number =
            (read . Tx.unpack) <$> psym (Tx.all isDigit)
        make tag sy =
            make' tag sy id
        make' tag sy f x y =
            create 
                Object
                tag 
                ( (_position .~ (x,y)) 
                . (_symbol .~ sy)
                . f)
