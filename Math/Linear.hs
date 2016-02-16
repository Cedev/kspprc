module Math.Linear where

import Prelude hiding (sum)

import Data.Foldable

import Control.Applicative

dot :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot x y = sum $ liftA2 (*) x y

proj :: (Fractional a, Foldable f, Applicative f) => f a -> f a -> f a
proj s v = (v `dot` s/s `dot` s) *^ s

towards :: (Fractional a, Foldable f, Applicative f, Eq (f a)) => f a -> f a -> Bool
towards s v = v /= zero && proj s v == v

zero :: (Num a, Applicative f) => f a
zero = pure 0

(*^) :: (Num a, Functor f) => a -> f a -> f a
c *^ f = (c*) <$> f

infixr 7 *^

(^+^) :: (Num a, Applicative f) => f a -> f a -> f a
x ^+^ y = liftA2 (+) x y

infixl 6 ^+^
