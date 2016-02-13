module Math.Linear where

import Prelude hiding (sum)

import Data.Foldable

import Control.Applicative

dot :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot x y = sum $ (*) <$> x <*> y

proj :: (Fractional a, Foldable f, Applicative f) => f a -> f a -> f a
proj s v = ((v `dot` s/s `dot` s)*) <$> s

towards :: (Fractional a, Foldable f, Applicative f, Eq (f a)) => f a -> f a -> Bool
towards s v = v /= pure 0 && proj s v == v
