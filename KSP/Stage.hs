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

import Debug.Trace

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

evaluate maneuvers = Rocketry.evaluate_stage maneuvers . convert_stage &&& id

stage_metric = Rocketry.stage_specific_impulse . evaluation &&& Down . total_parts . evaluated_stage &&& Down . total_cost . evaluated_stage

by_stage_metric = (<) `on` stage_metric

suboptimal_engine_stage :: (EvaluatedStage -> EvaluatedStage -> Bool) -> (Stage -> EvaluatedStage) -> [Part Rational] -> Rational -> Part Rational -> EvaluatedStage
suboptimal_engine_stage admit evaluate part_db payload engine = snd $ visitOn_ (expansion_order . snd) better_than expand ((parts, admit empty_stage), empty_stage)
    where
        expansion_order = total_dry_mass . evaluated_stage &&& Down . stage_metric

        better_than :: (([Part Rational], EvaluatedStage -> Bool), EvaluatedStage) -> (([Part Rational], EvaluatedStage -> Bool), EvaluatedStage) -> Bool
        better_than ((_, test), _) (_, s) = test s

        expand :: (([Part Rational], EvaluatedStage -> Bool), EvaluatedStage) -> [(([Part Rational], EvaluatedStage -> Bool), EvaluatedStage)]
        expand ((parts, _), (_, stage)) = [
            let s = evaluate . extend_symetric (head p) $ stage
            in ((p, admit s), s) | p <- (init . tails) parts]

        parts :: [Part Rational]
        parts = sortOn (Down . dry_mass) . filter (fuels stage_thruster) $ part_db
        
        stage_thruster = (fromJust . thruster) engine
        
        empty_stage :: EvaluatedStage
        empty_stage = evaluate . extend_symetric engine $ stage payload stage_thruster []

optimal_engine_stage :: (Stage -> EvaluatedStage) -> [Part Rational] -> Rational -> Part Rational -> EvaluatedStage
optimal_engine_stage = suboptimal_engine_stage by_stage_metric

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
                oes = optimal_engine_stage (evaluate maneuvers) part_db
                s = optimal_stage oes part_db payload
                delta_v_s = Rocketry.stage_delta_v . evaluation $ s

after :: [Rocketry.Maneuver (Environment Rational)] -> EvaluatedStage -> [Rocketry.Maneuver (Environment Rational)]
after maneuvers = Rocketry.after maneuvers . Rocketry.stage_delta_v . evaluation

optimal_mission_after :: [Part Rational] -> EvaluatedStage -> [Rocketry.Maneuver (Environment Rational)] -> [EvaluatedStage]
optimal_mission_after part_db s maneuvers = optimal_mission_stages part_db (total_mass . evaluated_stage $ s) (after maneuvers s)

optimal_next_stage :: [Part Rational] -> EvaluatedStage -> EvaluatedStage
optimal_next_stage part_db stage = s
    where
        maneuver = Rocketry.forever . Rocketry.maneuver . last . Rocketry.maneuvers . evaluation $ stage
        dv = Rocketry.evaluate_stage [maneuver] . convert_stage &&& id
        oes = optimal_engine_stage dv part_db
        payload = total_mass . evaluated_stage $ stage
        s = optimal_stage oes part_db payload

rebalanced_mission_stages :: [Part Rational] -> Rational -> [Rocketry.Maneuver (Environment Rational)] -> [EvaluatedStage]
rebalanced_mission_stages part_db = go
    where
        go _ [] = []
        go payload maneuvers =
            if delta_v_s <= 0
            then []
            else s : go (total_mass . evaluated_stage $ s) (Rocketry.after maneuvers delta_v_s)
            where
                rebalancing_engine_stage = suboptimal_engine_stage rebalancing_comparisson (evaluate maneuvers) part_db
                s = optimal_stage rebalancing_engine_stage part_db payload
                delta_v_s = Rocketry.stage_delta_v . evaluation $ s

        rebalancing_comparisson s0 = comparisson
            where
                comparisson s1 = by_stage_metric s0 s1 || Rocketry.marginal_stage_specific_impulse (evaluation s0) (evaluation s1) > optimal_next_specific_impulse
                optimal_next_specific_impulse = trace_ $ Rocketry.stage_specific_impulse . evaluation . optimal_next_stage part_db $ s0

trace_ :: Show a => a -> a
trace_ x = traceShow x x

shifted_mission_stages :: [Part Rational] -> Rational -> [Rocketry.Maneuver (Environment Rational)] -> [EvaluatedStage]
shifted_mission_stages part_db payload maneuvers = go payload maneuvers (optimal_mission_stages part_db payload maneuvers)
    where
        go :: Rational -> [Rocketry.Maneuver (Environment Rational)] -> [EvaluatedStage] -> [EvaluatedStage]
        go _       []        _              = []
        go _       _         []             = []
        go _       _         [s]            = [s]
        go payload maneuvers (s0 : s1 : ss) = s : go (total_mass . evaluated_stage $ s) (after maneuvers s) (tail plan)
            where
                isp_s1 = Rocketry.stage_specific_impulse (evaluation s1)
                rebalancing_comparisson =
                    if Rocketry.stage_specific_impulse (evaluation s0) >= isp_s1
                    then \x y -> by_stage_metric x y || Rocketry.marginal_stage_specific_impulse (evaluation x) (evaluation y) >  isp_s1
                    else \x y -> by_stage_metric x y && Rocketry.marginal_stage_specific_impulse (evaluation x) (evaluation y) >= isp_s1
                s0' = optimal_stage (suboptimal_engine_stage rebalancing_comparisson (evaluate maneuvers) part_db) part_db payload
                ss' = optimal_mission_after part_db s0' maneuvers
                plan :: [EvaluatedStage]
                plan = minimumOn (total_mass . evaluated_stage . last) $ [s0 : s1 : ss, s0' : ss']
                s :: EvaluatedStage
                s = head plan
