{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Frontier.Feature.Compose
    ((:<+>)()
    ,(<+>)
    ) where

import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Entity (Seed (Blank))

data (:<+>) a b c = (:<+>) (a c) (b c) deriving (Show, Eq)
infixl 5 :<+>

split :: forall a b c m.
         Monad m
      => Feature a
      -> Feature b
      -> (ActionT (a :<+> b) m () -> c)
      -> (ActionT a m () -> c
         ,ActionT b m () -> c
         )
split f g a =
    (a . transActionT (:<+> blank') fst'
    ,a . transActionT (blank :<+>) snd'
    ) where
        blank :: a d
        blank = componentFor f Blank

        blank' :: b d
        blank' = componentFor g Blank

        fst' :: (:<+>) a b d -> a d
        fst' (x :<+> _) = x

        snd' :: (:<+>) a b d -> b d
        snd' (_ :<+> x) = x

(<+>) :: Feature a ->  Feature b -> Feature (a :<+> b)
(<+>) f g = Feature
    {componentFor = \x ->
        componentFor f x :<+> componentFor g x
    ,initItems =
        initItems f ++ initItems g
    ,symbol = \(x :<+> y) ->
        symbol f x ++ symbol g y
    ,command = \x y ->
        let (y', y'') = split f g y
        in command f x y' ++ command g x y''
    ,doTurn = \(x :<+> y) z ->
        let (z', z'') = split f g z
        in doTurn f x z' ++ doTurn g y z''
    ,eq = \(x :<+> y) (x' :<+> y') ->
        eq f x x' && eq g y y'
    ,partialUpdate = \(x :<+> y) (x' :<+> y') ->
        partialUpdate f x x' :<+> partialUpdate g y y'
    }
infixl 5 <+>
