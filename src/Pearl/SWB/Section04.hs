{-# LANGUAGE TypeOperators #-}

module Pearl.SWB.Section04 where

import Pearl.SWB.Section03 (Gf(..),Lf(..), fold, unfold, KList(..), SList(..))

data l :/\: r = As { outL :: l , outR :: r }
data l :\/: r = Paws l                       -- :||_||:
              | Play r                       -- :||>:


(/\) :: (     x -> a) -> (     x -> b) -> (x -> a :/\: b)
(\/) :: (a -> x     ) -> (b -> x     ) -> (     a :\/: b -> x)

(/\) f g    x     = As (f x) (g x)
(\/) f g (Paws a) =     f a
(\/) f g (Play b) =           g b


para      :: (Functor f) => (     f (Lf f :/\: a) -> a) -> (     Lf f -> a)
para'     :: (Functor f) => (     f (Lf f :/\: a) -> a) -> (     Lf f -> a)
apo       :: (Functor f) => (a -> f (Gf f :\/: a)     ) -> (a -> Gf f)
apo'      :: (Functor f) => (a -> f (Gf f :\/: a)     ) -> (a -> Gf f)

para  f   = f .    fmap (     id                   /\ para f) . insideI
para' f   = outR . fold ((In . fmap outL)          /\      f)
apo   f   = OutO . fmap (     id                   \/ apo  f) . f
apo'  f   =        unfold ((fmap (Paws) . insideO) \/      f) . Play


type FixO f a =  a :\/: f a
type FixI f a =  a :/\: f a



-- type transformations:
--


step0  :: (Functor f, Functor v) => (f (Gf v) -> v (Gf v  :\/: f (Gf  v))                                   )                        -> Lf f -> Gf v
step10 :: (Functor x, Functor f) => (            f (Lf f  :/\: x (Lf  f)) -> x (Lf f)                       )                        -> Lf f -> Gf x
step20 :: (Functor f, Functor v) => (            f (Lf f  :/\:   (Gf  v)) -> v (Gf v :\/: f (Lf f :/\: Gf v))                      ) -> Lf f -> Gf v
step30 :: (Functor f, Functor v) => (                                        v (Lf v :/\: f (Gf f :\/: Lf v)) -> f (Gf f :\/: Lf v)) -> Lf v -> Gf f

step0 c = fold (apo c)
step1 c =       apo c
step2 c =           c


step10 a = unfold (para a)
step11 a =         para a
step12 a =              a



step20 c = para (apo c)


step30 a = apo (para a)

