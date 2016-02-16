module KSP.Assembly where

import Data.Maybe

import Math.Linear

import KSP.Data.Parts

import qualified Engineering.Rocketry as Rocketry

import KSP.Data.Environment

import KSP.Evaluation
import KSP.Stage

fuels :: Thruster Rational -> Part Rational -> Bool
fuels engine part = towards (propellant engine) (capacity part) && isNothing (thruster part)

-- If a part's asymetrical we have to add two of them the first time we add one.
extend_symetric :: Part Rational -> Stage -> Stage
extend_symetric p s =
    if (asymetrical . geometry) p && notElem p (components s)
    then (extend p . extend p) s
    else extend p s

data StageAssembly = StageAssembly {
    get_stage :: EvaluatedStage,
    add_part :: Part Rational -> StageAssembly,
    allowed_parts :: [Part Rational]
}

data VehicleAssembly = VehicleAssembly {
    done :: Bool,
    add_stage :: EvaluatedStage -> VehicleAssembly,
    stage_assembly :: StageAssembly
}

type Maneuver = Rocketry.Maneuver (Environment Rational)

after :: [Maneuver] -> EvaluatedStage -> [Maneuver]
after maneuvers = Rocketry.after maneuvers . Rocketry.stage_delta_v . evaluation

mk_stage_assembly :: (Part Rational -> Stage -> Stage) -> [Part Rational] -> (Stage -> EvaluatedStage) -> Stage -> StageAssembly
mk_stage_assembly extend parts evaluate = go 
    where
        go stage = StageAssembly {
            get_stage = evaluate stage,
            add_part = go . flip extend stage,
            allowed_parts = parts
        }

mission_vehicle_assembly :: Rational -> [Maneuver] -> [Part Rational] -> VehicleAssembly
mission_vehicle_assembly payload maneuvers parts = VehicleAssembly {
    done = null maneuvers,
    add_stage = \s -> mission_vehicle_assembly (total_mass . evaluated_stage $ s) (after maneuvers s) parts,
    stage_assembly = mk_stage_assembly extend_symetric parts (evaluate maneuvers) (stage payload [])
}
