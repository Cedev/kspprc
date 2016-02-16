module KSP.Evaluation where

import Control.Arrow

import Math.Interpolation

import qualified Engineering.Rocketry as Rocketry

import KSP.Data.Bodies
import KSP.Data.Environment
import KSP.Data.Parts

import KSP.Stage


convert_stage :: Stage -> Rocketry.Stage (Environment Rational)
convert_stage s = Rocketry.Stage {
    Rocketry.payload_mass = payload_mass s,
    Rocketry.total_mass   = total_mass s,
    Rocketry.dry_mass     = total_dry_mass s,
    Rocketry.isp = fromRational . (*g) . piecewise_linear (isp $ engine s) . atmospheric_pressure,
    Rocketry.mass_flow = const . fromRational . mass_flow . engine $ s
}

type EvaluatedStage = (Rocketry.StageEvaluation (Environment Rational), Stage)

evaluated_stage :: EvaluatedStage -> Stage
evaluated_stage = snd

evaluation :: EvaluatedStage -> Rocketry.StageEvaluation (Environment Rational)
evaluation = fst

evaluate maneuvers = Rocketry.evaluate_stage maneuvers . convert_stage &&& id
