module KSP.Stage where

import Prelude hiding (sum, mapM_)

import Data.Foldable

import Data.List
import Data.Maybe
import Data.Ord

import Control.Applicative
import Control.Arrow

import Data.List.Decorate
import Math.Linear
import Math.Interpolation

import qualified Engineering.Rocketry as Rocketry

import KSP.Data.Bodies
import KSP.Data.Environment
import KSP.Data.Parts
import KSP.Data.Resources

fuels :: Thruster Rational -> Part Rational -> Bool
fuels engine part = towards (propellant engine) (capacity part) && isNothing (thruster part)

vac_isp = ($ 0) . piecewise_linear . isp
engine_isp = vac_isp

mass_flow s = (vac_thrust . engine) s / (g * (vac_isp . engine) s)  -- kN / (m/s) = ton / s

data Stage = Stage {
    engine :: Thruster Rational,
    total_dry_mass :: Rational,
    total_capacity :: ResourceVector Rational,
    total_mass :: Rational,
    total_parts :: Int,
    total_cost :: Rational,
    components :: [Part Rational],
    payload_mass :: Rational
}


data Evaluated s = Evaluated {
    delta_v :: Double,
    stage_specific_implulse :: Double,
    evaluated_stage :: s
}

stage_delta_v :: Stage -> Double
stage_delta_v s = log ((fromRational . total_mass) s/(fromRational . total_dry_mass) s) * (fromRational . engine_isp . engine) s * g

evaluate :: (Stage -> Double) -> Stage -> Evaluated Stage
evaluate dv s = Evaluated {
    delta_v = delta_v,
    stage_specific_implulse = stage_specific_implulse,
    evaluated_stage = s
}
    where
        delta_v = dv s
        stage_specific_implulse = delta_v / log ((fromRational . total_mass) s / (fromRational . payload_mass) s)

stage :: Rational -> Thruster Rational -> [Part Rational] -> Stage
stage payload engine components = Stage {
    engine = engine,
    total_dry_mass = total_dry_mass,
    total_capacity = total_capacity,
    total_mass = total_mass,
    total_parts = total_parts,
    total_cost = total_cost,
    components = components,
    payload_mass = payload
}
    where
        total_dry_mass = payload + (sum . map dry_mass) components
        total_capacity = foldl (liftA2 (+)) zero_resource . map capacity $ components
        total_mass = total_dry_mass + resource_density `dot` total_capacity
        total_parts = length components
        total_cost = resource_cost `dot` total_capacity + (sum . map dry_cost) components

extend :: Part Rational -> Stage -> Stage
extend p s = s' where s' = Stage {
    engine = engine s,
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

{- marginal stage specific impulse is
    delta_v now - delta_v before
    ----------------------------
     log (mass now/mass before)

    as long as the marginal stage specific impulse is better than the
    specific impulse of the best possible stage in the current environment
    it's probably better to keep extending the current stage
-}


stage_metric = stage_specific_implulse &&& Down . total_parts . evaluated_stage &&& Down . total_cost . evaluated_stage

        
optimal_engine_stage :: (Stage -> Double) -> [Part Rational] -> Rational -> Part Rational -> Evaluated Stage
optimal_engine_stage dv part_db payload engine = expand empty_stage parts []
    where
        -- s0 is the new best stage, or it would have been filtered out.
        go :: Evaluated Stage -> [([Part Rational], Evaluated Stage)] -> Evaluated Stage
        go best_stage ((p0, s0) : ss) = expand s0 p0 ss
        go best_stage []              = best_stage
        
        expand :: Evaluated Stage -> [Part Rational] -> [([Part Rational], Evaluated Stage)] -> Evaluated Stage
        expand best_stage parts ss = go best_stage sorted
            where
                increasing = increasingOn (stage_metric . snd) sorted
                sorted = sortOn (expansion_order . snd) filtered
                filtered = filter (\(_, s) -> stage_metric s > stage_metric best_stage) expanded
                expanded = [(p, evaluate dv . extend_symetric (head p) . evaluated_stage $ best_stage) | p <- (init . tails) parts] ++ ss
                
        expansion_order = total_dry_mass . evaluated_stage &&& Down . stage_metric

        parts :: [Part Rational]
        parts = sortOn (Down . dry_mass) . filter (fuels stage_thruster) $ part_db
        
        stage_thruster = (fromJust . thruster) engine
        
        empty_stage :: Evaluated Stage
        empty_stage = evaluate dv . extend_symetric engine $ stage payload stage_thruster []
        
optimal_stage :: (Rational -> Part Rational -> Evaluated Stage) -> [Part Rational] -> Rational -> Evaluated Stage
optimal_stage optimal_engine_stage part_db payload = head . sortOn (Down . stage_metric) $ engine_stages
    where
        engine_stages = map (optimal_engine_stage payload) engines
        engines = filter (isJust . thruster) part_db

optimal_stages' :: (Rational -> Evaluated Stage) -> Rational -> [Evaluated Stage]
optimal_stages' optimal_stage = go
    where
        go payload = s0 : (go . total_mass . evaluated_stage) s0
            where
                s0 = optimal_stage payload

optimal_stages :: (Stage -> Double) -> [Part Rational] -> Rational -> Rational -> [Evaluated Stage]
optimal_stages dv part_db payload mission_delta_v = head . dropWhile ((< fromRational mission_delta_v) . sum . map delta_v) . inits $ optimal_stages' os payload
    where
        os = optimal_stage oes part_db
        oes = optimal_engine_stage dv part_db

convert_stage :: Stage -> Rocketry.Stage (Environment Rational)
convert_stage s = Rocketry.Stage {
    Rocketry.initial_mass = total_dry_mass s,
    Rocketry.final_mass = total_mass s,
    Rocketry.isp = fromRational . (*g) . piecewise_linear (isp $ engine s) . atmospheric_pressure,
    Rocketry.mass_flow = const . fromRational $ mass_flow s
}

optimal_mission_stages :: [Part Rational] -> Rational -> [Rocketry.Maneuver (Environment Rational)] -> [Evaluated Stage]
optimal_mission_stages part_db = go
    where
        go _ [] = []
        go payload maneuvers =
            if delta_v s <= 0
            then []
            else s : go (total_mass . evaluated_stage $ s) (Rocketry.after maneuvers . delta_v $ s)
            where
                dv = Rocketry.stage_delta_v . Rocketry.evaluate_stage maneuvers . convert_stage
                oes = optimal_engine_stage dv part_db
                s = optimal_stage oes part_db payload
