{-# LANGUAGE GADTs #-}

module Mimir.Free where

import Control.Monad (ap, liftM)

data Free f r where
    Pure :: r -> Free f r
    Free :: f x -> (x -> Free f a) -> Free f a

instance Functor (Free f) where
    fmap = liftM

instance Applicative (Free f) where
    pure = return
    (<*>) = ap

instance Monad (Free f) where
    return = Pure
    (>>=) (Pure r) f = f r
    (>>=) (Free fx k') k = Free fx (\x -> k' x >>= k)

liftF :: f a -> Free f a
liftF = flip Free return
