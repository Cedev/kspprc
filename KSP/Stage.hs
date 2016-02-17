module KSP.Stage where

import Prelude hiding (sum, mapM_)

import Data.Foldable

import Data.Maybe
import Data.Monoid

import Control.Applicative

import qualified Data.Map.Strict as Map

import Math.Linear

import KSP.Data.Parts
import KSP.Data.Resources

type Bag a = Map.Map a 

data Stage = Stage {
    engine :: Thruster Rational,
    total_dry_mass :: Rational,
    total_capacity :: ResourceVector Rational,
    total_mass :: Rational,
    total_parts :: Integer,
    total_engines :: Integer,
    total_cost :: Rational,
    components :: Map.Map (Part Rational) Integer,
    payload_mass :: Rational
} deriving (Eq, Ord, Show, Read)

instance Monoid Stage where
    mempty = Stage {
        engine         = mempty,
        total_dry_mass = 0,
        total_capacity = zero,
        total_mass     = 0,
        total_parts    = 0,
        total_engines  = 0,
        total_cost     = 0,
        components     = Map.empty,
        payload_mass   = 0
    }

    x `mappend` y = Stage {
        engine         = engine x         <>  engine y,
        total_dry_mass = total_dry_mass x +   total_dry_mass y,
        total_capacity = total_capacity x ^+^ total_capacity y,
        total_mass     = total_mass x     +   total_mass y,
        total_parts    = total_parts x    +   total_parts y,
        total_engines  = total_engines x  +   total_engines y,
        total_cost     = total_cost x     +   total_cost y,
        components = Map.unionWith (+) (components x) (components y),
        payload_mass   = payload_mass x   +   payload_mass y
    }

quantities = Map.toList

filter_components :: (k -> Bool) -> Map.Map k a -> Map.Map k a
filter_components f = Map.filterWithKey (const . f)

scale :: Num n => (n -> a -> b) -> (k -> a) -> Map.Map k Integer -> [b]
scale scale_by f = map (\(k, v) -> scale_by (fromIntegral v) (f k)) . Map.toList

stuff :: (Ord k, Num n) => n -> k -> Map.Map k n -> Map.Map k n
stuff n k = Map.insertWith (+) k n

empty_stage :: Rational -> Stage
empty_stage payload = mempty {
    total_dry_mass = payload,
    total_mass = payload,
    payload_mass = payload
}

stage :: Rational -> [Part Rational] -> Stage
stage payload = mconcat . (empty_stage payload :) . map (part_stage 1)

part_stage :: Integer -> Part Rational -> Stage
part_stage n p = Stage {
    engine = maybe mempty (thruster_cluster n') $ thruster p,
    total_dry_mass = n' * dry_mass p,
    total_capacity = n' *^ capacity p,
    total_mass     = n' * dry_mass p + resource_density `dot` new_capacity,
    total_parts    = n,
    total_engines  = if isJust . thruster $ p then n else 0,
    total_cost     = n' * dry_cost p + resource_cost `dot` new_capacity,
    components     = Map.singleton p n,
    payload_mass   = 0
}
    where
        n' = fromIntegral n
        new_capacity = n' *^ capacity p

extend :: Integer -> Part Rational -> Stage -> Stage
extend n p s = part_stage n p <> s
