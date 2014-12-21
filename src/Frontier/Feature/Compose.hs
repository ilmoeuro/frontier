{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Frontier.Feature.Compose
    ((:<+>)()
    ,(<+>)
    ) where

import Control.Monad.Trans.Free
import Frontier.Feature
import Frontier.Feature.Action
import Frontier.Feature.Entity (Seed (Blank))

data (:<+>) a b c = (:<+>) (a c) (b c) deriving (Show, Eq)
infixl 5 :<+>

promote :: forall a b d.
           (forall c. a c -> b c)
        -> (forall c. b c -> a c)
        -> ActionM a d
        -> ActionM b d
promote up down = iterTM $ \case
    (ShortDescription x n) ->
        shortDescription x >> n
    (Target (InventoryItem a) n) ->
        target (InventoryItem $ \i -> promoteTarget $ a (down i)) >> n
    (Target (NearObject a) n) ->
        target (NearObject $ \o -> promoteTarget $ a (down o)) >> n
    (Target (EmptySpace a) n) ->
        target (EmptySpace $ promoteTarget a) >> n
    (UseItem c i n) ->
        useItem c (up i) >> n
    (YieldItem i n) ->
        yieldItem (up i) >> n
    (Me n) ->
        (down `fmap` me) >>= n
    (Move d o n) ->
        move d (up o) >> n
  where
    promoteTarget :: ActionM a (Outcome a c')
                  -> ActionM b (Outcome b c')
    promoteTarget a = promote up down $ fmapOutcome up `fmap` a

    fmapOutcome :: (a c' -> b c')
                -> Outcome a c'
                -> Outcome b c'
    fmapOutcome _ Retain = Retain
    fmapOutcome f (Modify x) = Modify (f x)
    fmapOutcome f (ReplaceWith (x,y)) = ReplaceWith (f x,y)
    fmapOutcome _ Destroy = Destroy

split :: forall a b c.
         Feature a
      -> Feature b
      -> (Action (a :<+> b) -> c)
      -> (Action a -> c
         ,Action b -> c
         )
split f g a =
    (a . promote (:<+> blank') fst'
    ,a . promote (blank :<+>) snd'
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
(<+>) f g (ComponentFor x)
    = f (ComponentFor x) :<+> g (ComponentFor x)
(<+>) f g InitItems
    = f InitItems ++ g InitItems
(<+>) f g (Symbol (x :<+> y))
    = f (Symbol x) ++ g (Symbol y)
(<+>) f g (Command x y)
    = let (y', y'') = split f g y in f (Command x y') ++ g (Command x y'')
(<+>) f g (DoTurn (x :<+> y) z)
    = let (z', z'') = split f g z in f (DoTurn x z') ++ g (DoTurn y z'')
(<+>) f g (Eq (x :<+> y) (x' :<+> y'))
    = f (Eq x x') && g (Eq y y')
(<+>) f g (PartialUpdate (x :<+> y) (x' :<+> y'))
    = f (PartialUpdate x x') :<+> g (PartialUpdate y y')
infixl 5 <+>
