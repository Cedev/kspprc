module KSP.Assembly where

import Data.Maybe

import qualified Data.Map.Strict as Map

import Math.Linear

import KSP.Data.Parts

import qualified Engineering.Rocketry as Rocketry

import KSP.Data.Environment

import KSP.Evaluation
import KSP.Stage

fuels :: Thruster Rational -> Part Rational -> Bool
fuels engine part = towards (propellant engine) (capacity part) && isNothing (thruster part)

-- If a part's asymetrical we have to add two of them the first time we add one.
extend_symetric :: Integer -> Part Rational -> Stage -> Stage
extend_symetric n p s =
    if n == 1 && (asymetrical . geometry) p && Map.notMember p (components s)
    then extend 2 p s
    else extend n p s

-- If a part's not an engine, we should add as many of them as we've added engines
extend_stacks :: Integer -> Part Rational -> Stage -> Stage
extend_stacks n p s =
    if isNothing . thruster $ p
    then extend_symetric (n*total_engines s) p s
    else extend_symetric n p s

data StageAssembly = StageAssembly {
    get_stage :: EvaluatedStage,
    add_parts :: Integer -> Part Rational -> StageAssembly,
    allowed_parts :: [Part Rational]
}

data VehicleAssembly = VehicleAssembly {
    done :: Bool,
    add_stage :: EvaluatedStage -> VehicleAssembly,
    stage_assembly :: StageAssembly
}

type Maneuver = Rocketry.Maneuver (Environment Rational)

add_part :: StageAssembly -> Part Rational -> StageAssembly
add_part s = add_parts s 1

after :: [Maneuver] -> EvaluatedStage -> [Maneuver]
after maneuvers = Rocketry.after maneuvers . Rocketry.stage_delta_v . evaluation

mk_stage_assembly :: (Integer -> Part Rational -> Stage -> Stage) -> [Part Rational] -> (Stage -> EvaluatedStage) -> Stage -> StageAssembly
mk_stage_assembly extend parts evaluate = go 
    where
        go stage = StageAssembly {
            get_stage = evaluate stage,
            add_parts = \n p -> go $ extend n p stage,
            allowed_parts = parts
        }

mission_vehicle_assembly :: Rational -> [Maneuver] -> [Part Rational] -> VehicleAssembly
mission_vehicle_assembly payload maneuvers parts = VehicleAssembly {
    done = null maneuvers,
    add_stage = \s -> mission_vehicle_assembly (total_mass . evaluated_stage $ s) (after maneuvers s) parts,
    stage_assembly = mk_stage_assembly extend_stacks parts (evaluate maneuvers) (stage payload [])
}
