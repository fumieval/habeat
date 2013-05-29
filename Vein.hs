{-# LANGUAGE Rank2Types #-}
module Vein (Vein(..)) where

import qualified Control.Category
import Control.Arrow
import Control.Applicative

newtype Vein m i o = Vein { runVein :: forall r. i -> (o -> Vein m i o -> m r) -> m r }

instance Control.Category.Category (Vein m) where
    id = Vein $ \x cont -> cont x Control.Category.id
    Vein f . Vein g = Vein $ \x cont -> g x $ \y g' -> f y $ \z f' -> cont z (f' Control.Category.. g')

instance Arrow (Vein m) where
    arr f = Vein $ \x cont -> cont (f x) (arr f)
    first (Vein f) = Vein $ \(x, y) cont -> f x $ \x' f' -> cont (x', y) (first f')
    second (Vein f) = Vein $ \(y, x) cont -> f x $ \x' f' -> cont (y, x') (second f')

instance Functor (Vein m a) where
    fmap f ar = ar >>> arr f

instance Applicative (Vein m a) where
    pure x = arr (const x)
    Vein ff <*> Vein fx = Vein $ \i cont -> ff i $ \f ff' -> fx i $ \x fx' -> cont (f x) (ff' <*> fx')

instance Num o => Num (Vein m i o) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
