module KSP.Calculator where

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
import Search

import qualified Engineering.Rocketry as Rocketry

import KSP.Data.Bodies
import KSP.Data.Environment
import KSP.Data.Parts
import KSP.Data.Resources

import KSP.Assembly
import KSP.Evaluation
import KSP.Stage

stage_metric = Rocketry.stage_specific_impulse . evaluation &&& Down . total_parts . evaluated_stage &&& Down . total_cost . evaluated_stage

by_stage_metric = (<) `on` stage_metric

suboptimal_engine_stage :: (EvaluatedStage -> EvaluatedStage -> Bool) -> StageAssembly -> Part Rational -> EvaluatedStage
suboptimal_engine_stage admit sa engine = get_stage . snd $ visitOn_ (expansion_order . get_stage . snd) better_than expand (state parts empty_stage)
    where
        expansion_order = total_dry_mass . evaluated_stage &&& Down . stage_metric

        better_than :: (([Part Rational], EvaluatedStage -> Bool), StageAssembly) -> (([Part Rational], EvaluatedStage -> Bool), StageAssembly) -> Bool
        better_than ((_, test), _) (_, s) = test . get_stage $ s

        expand :: (([Part Rational], EvaluatedStage -> Bool), StageAssembly) -> [(([Part Rational], EvaluatedStage -> Bool), StageAssembly)]
        expand ((parts, _), stage) = [
            let s = add_part stage (head p)
            in state p s | p <- (init . tails) parts]

        state :: [Part Rational] -> StageAssembly -> (([Part Rational], EvaluatedStage -> Bool), StageAssembly)
        state parts s = ((parts, admit (get_stage s)), s)

        parts :: [Part Rational]
        parts = sortOn (Down . dry_mass) . filter (fuels stage_thruster) $ allowed_parts sa
        
        stage_thruster = (fromJust . thruster) engine
        
        empty_stage :: StageAssembly
        empty_stage = add_part sa engine

optimal_engine_stage :: StageAssembly -> Part Rational -> EvaluatedStage
optimal_engine_stage = suboptimal_engine_stage by_stage_metric

optimal_stage :: (StageAssembly -> Part Rational -> EvaluatedStage) -> StageAssembly -> EvaluatedStage
optimal_stage optimal_engine_stage sa = head . sortOn (Down . stage_metric) $ engine_stages
    where
        engine_stages = map (optimal_engine_stage sa) engines
        engines = filter (isJust . thruster) $ allowed_parts sa

optimal_mission_stages :: VehicleAssembly -> [EvaluatedStage]
optimal_mission_stages va =
    if done va || delta_v_s <= 0
    then []
    else s : optimal_mission_stages (add_stage va s)
    where
        s = optimal_stage optimal_engine_stage (stage_assembly va)
        delta_v_s = Rocketry.stage_delta_v . evaluation $ s

shifted_mission_stages :: Int -> VehicleAssembly -> [EvaluatedStage]
shifted_mission_stages n va = go n va (optimal_mission_stages va)
    where
        go :: Int -> VehicleAssembly -> [EvaluatedStage] -> [EvaluatedStage]
        go _ _       []             = []
        go _ _       [s]            = [s]
        go i va      (s0 : s1 : ss) =
            if i == 0 || s == s0
            then s : go n (add_stage va s) (tail plan)
            else go (i-1) va plan
            where
                isp_s1 = Rocketry.stage_specific_impulse (evaluation s1)
                rebalancing_comparisson =
                    if Rocketry.stage_specific_impulse (evaluation s0) >= isp_s1
                    then \x y -> by_stage_metric x y || Rocketry.marginal_stage_specific_impulse (evaluation x) (evaluation y) >  isp_s1
                    else \x y -> by_stage_metric x y && Rocketry.marginal_stage_specific_impulse (evaluation x) (evaluation y) >= isp_s1
                s0' = optimal_stage (suboptimal_engine_stage rebalancing_comparisson) (stage_assembly va)
                ss' = optimal_mission_stages (add_stage va s0')
                plan :: [EvaluatedStage]
                plan = minimumOn (total_mass . evaluated_stage . last) $ [s0 : s1 : ss, s0' : ss']
                s :: EvaluatedStage
                s = head plan
