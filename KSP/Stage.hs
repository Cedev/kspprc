module KSP.Stage where

import Prelude hiding (sum, mapM_)

import Data.Foldable

import Data.Function
import Data.List
import Data.Maybe
import Data.Ord

import Control.Applicative
import Control.Arrow

import Data.List.Decorate
import Math.Linear
import Math.Interpolation
import Search

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

type EvaluatedStage = (Rocketry.StageEvaluation (Environment Rational), Stage)

evaluated_stage :: EvaluatedStage -> Stage
evaluated_stage = snd

evaluation :: EvaluatedStage -> Rocketry.StageEvaluation (Environment Rational)
evaluation = fst

stage_metric = Rocketry.stage_specific_impulse . evaluation &&& Down . total_parts . evaluated_stage &&& Down . total_cost . evaluated_stage
        
optimal_engine_stage :: (Stage -> EvaluatedStage) -> [Part Rational] -> Rational -> Part Rational -> EvaluatedStage
optimal_engine_stage evaluate part_db payload engine = snd $ visitOn_ (expansion_order . snd) (better_than `on` snd) expand (parts, empty_stage)
    where
        expansion_order = total_dry_mass . evaluated_stage &&& Down . stage_metric

        better_than :: EvaluatedStage -> EvaluatedStage -> Bool
        better_than best_stage s = stage_metric s > stage_metric best_stage

        expand :: ([Part Rational], EvaluatedStage) -> [([Part Rational], EvaluatedStage)]
        expand (parts, (_, stage)) =  [(p, evaluate . extend_symetric (head p) $ stage) | p <- (init . tails) parts]

        parts :: [Part Rational]
        parts = sortOn (Down . dry_mass) . filter (fuels stage_thruster) $ part_db
        
        stage_thruster = (fromJust . thruster) engine
        
        empty_stage :: EvaluatedStage
        empty_stage = evaluate . extend_symetric engine $ stage payload stage_thruster []
        
optimal_stage :: (Rational -> Part Rational -> EvaluatedStage) -> [Part Rational] -> Rational -> EvaluatedStage
optimal_stage optimal_engine_stage part_db payload = head . sortOn (Down . stage_metric) $ engine_stages
    where
        engine_stages = map (optimal_engine_stage payload) engines
        engines = filter (isJust . thruster) part_db

convert_stage :: Stage -> Rocketry.Stage (Environment Rational)
convert_stage s = Rocketry.Stage {
    Rocketry.payload_mass = payload_mass s,
    Rocketry.total_mass   = total_mass s,
    Rocketry.dry_mass     = total_dry_mass s,
    Rocketry.isp = fromRational . (*g) . piecewise_linear (isp $ engine s) . atmospheric_pressure,
    Rocketry.mass_flow = const . fromRational $ mass_flow s
}

optimal_mission_stages :: [Part Rational] -> Rational -> [Rocketry.Maneuver (Environment Rational)] -> [EvaluatedStage]
optimal_mission_stages part_db = go
    where
        go _ [] = []
        go payload maneuvers =
            if delta_v_s <= 0
            then []
            else s : go (total_mass . evaluated_stage $ s) (Rocketry.after maneuvers delta_v_s)
            where
                dv = Rocketry.evaluate_stage maneuvers . convert_stage &&& id
                oes = optimal_engine_stage dv part_db
                s = optimal_stage oes part_db payload
                delta_v_s = Rocketry.stage_delta_v . evaluation $ s
