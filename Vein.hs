module Vein (Vein(..)) where

import qualified Control.Category
import Control.Arrow
import Control.Applicative

newtype Vein m i o = Vein { runVein :: i -> m (o, Vein m i o) }

instance Monad m => Control.Category.Category (Vein m) where
    id = Vein $ \x -> return (x, Control.Category.id)
    Vein f . Vein g = Vein $ \x -> do
        (y, b) <- g x
        (z, a) <- f y
        return $! (z, a Control.Category.. b)

instance Monad m => Arrow (Vein m) where
    arr f = Vein $ \x -> return (f x, arr f)
    first (Vein f) = Vein $ \(x, y) -> do
        (x', a) <- f x
        return $! ((x', y), first a)
    second (Vein f) = Vein $ \(y, x) -> do
        (x', a) <- f x
        return $! ((y, x'), second a)

instance Monad m => Functor (Vein m a) where
    fmap f ar = ar >>> arr f

instance Monad m => Applicative (Vein m a) where
    pure x = arr (const x)
    Vein ff <*> Vein fx = Vein $ \i -> do
        (f, ff') <- ff i
        (x, fx') <- fx i
        return $! (f x, ff' <*> fx')

instance (Monad m, Num o) => Num (Vein m i o) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
