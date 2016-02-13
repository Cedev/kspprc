module KSP.Stage where

import Prelude hiding (sum, mapM_)

import Data.Foldable

import Data.List
import Data.Maybe
import Data.Ord

import Control.Applicative
import Control.Arrow

import Math.Linear

import KSP.Data.Bodies
import KSP.Data.Parts
import KSP.Data.Resources

fuels :: Thruster Rational -> Part Rational -> Bool
fuels engine part = towards (propellant engine) (capacity part) && isNothing (thruster part)


engine_isp = snd . head . isp

data Stage = Stage {
    engine :: Thruster Rational,
    delta_v :: Double,
    stage_specific_implulse :: Double,
    total_dry_mass :: Rational,
    total_capacity :: ResourceVector Rational,
    total_mass :: Rational,
    total_parts :: Int,
    total_cost :: Rational,
    components :: [Part Rational],
    payload_mass :: Rational
}

stage :: Rational -> Thruster Rational -> [Part Rational] -> Stage
stage payload engine components = Stage {
    engine = engine,
    delta_v = delta_v,
    stage_specific_implulse = stage_specific_implulse,
    total_dry_mass = total_dry_mass,
    total_capacity = total_capacity,
    total_mass = total_mass,
    total_parts = total_parts,
    total_cost = total_cost,
    components = components,
    payload_mass = payload
}
    where
        delta_v = log (fromRational total_mass/fromRational total_dry_mass) * (fromRational . engine_isp) engine * g
        stage_specific_implulse = delta_v / log (fromRational total_mass / fromRational payload)
        total_dry_mass = payload + (sum . map dry_mass) components
        total_capacity = foldl (liftA2 (+)) zero_resource . map capacity $ components
        total_mass = total_dry_mass + resource_density `dot` total_capacity
        total_parts = length components
        total_cost = resource_cost `dot` total_capacity + (sum . map dry_cost) components

extend :: Part Rational -> Stage -> Stage
extend p s = s' where s' = Stage {
    engine = engine s,
    delta_v = log ((fromRational . total_mass) s'/(fromRational . total_dry_mass) s') * (fromRational . engine_isp . engine) s' * g,
    stage_specific_implulse = delta_v s' / log ((fromRational . total_mass) s' / (fromRational . payload_mass) s'),
    total_dry_mass = total_dry_mass s + dry_mass p,
    total_capacity = (+) <$> total_capacity s <*> capacity p,
    total_mass = total_mass s + dry_mass p + resource_density `dot` capacity p,
    total_parts = total_parts s + 1,
    total_cost = total_cost s + dry_cost p + resource_cost `dot` capacity p,
    components = p : components s,
    payload_mass = payload_mass s
}

-- If a part's asymetrical we have to add two of them the first time we add one.
extend_symetric :: Part Rational -> Stage -> Stage
extend_symetric p s =
    if (asymetrical . geometry) p && notElem p (components s)
    then (extend p . extend p) s
    else extend p s


stage_metric = stage_specific_implulse &&& Down . total_parts &&& Down . total_cost
        
increasingOn :: Ord b => (a -> b) -> [a] -> [a]
increasingOn f (x : xs) = x : go (f x) xs
    where
        go best (x : xs) = 
            if f x > best
            then x : go (f x) xs
            else go best xs
        go _ [] = []
increasingOn f [] = []
        
optimal_engine_stage :: Rational -> Part Rational -> [Part Rational] -> Stage
optimal_engine_stage payload engine part_db = expand empty_stage parts []
    where
        -- s0 is the new best stage, or it would have been filtered out.
        go :: Stage -> [([Part Rational], Stage)] -> Stage
        go best_stage ((p0, s0) : ss) = expand s0 p0 ss
        go best_stage []              = best_stage
        
        expand :: Stage -> [Part Rational] -> [([Part Rational], Stage)] -> Stage
        expand best_stage parts ss = go best_stage sorted
            where
                increasing = increasingOn (stage_metric . snd) sorted
                sorted = sortOn (expansion_order . snd) filtered
                filtered = filter (\(_, s) -> stage_metric s > stage_metric best_stage) expanded
                expanded = [(p, extend_symetric (head p) best_stage) | p <- (init . tails) parts] ++ ss
                
        expansion_order = total_dry_mass &&& Down . stage_metric

        parts :: [Part Rational]
        parts = sortOn (Down . dry_mass) . filter (fuels stage_thruster) $ part_db
        
        stage_thruster = (fromJust . thruster) engine
        
        empty_stage :: Stage
        empty_stage = extend_symetric engine $ stage payload stage_thruster []

        
optimal_stage :: Rational -> [Part Rational] -> Stage
optimal_stage payload part_db = head . sortOn (Down . stage_metric) $ engine_stages
    where
        engine_stages = [optimal_engine_stage payload engine part_db | engine <- engines]
        engines = filter (isJust . thruster) part_db
        
optimal_stages' :: Rational -> [Part Rational] -> [Stage]
optimal_stages' payload part_db = s0 : optimal_stages' (total_mass s0) part_db
    where
        s0 = optimal_stage payload part_db
        
optimal_stages :: Rational -> Rational -> [Part Rational] -> [Stage]
optimal_stages payload mission_delta_v part_db = head . dropWhile ((< fromRational mission_delta_v) . sum . map delta_v) . inits $ optimal_stages' payload part_db
