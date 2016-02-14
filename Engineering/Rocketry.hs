{-# LANGUAGE TypeFamilies #-}

module Engineering.Rocketry where

import Numeric.AD.Mode
import Numeric.AD.Newton.Double

data Stage e = Stage {
    initial_mass :: Rational,
    final_mass :: Rational,
    isp :: e -> Double,
    mass_flow :: e -> Double
}

data Maneuver e = Maneuver {
    delta_v :: Double,
    gravity :: Double,
    environment :: e
}

data StageEvaluation e = StageEvaluation {
    stage_delta_v :: Double,
    stage_gravity_delta_v :: Double,
    stage_burn_time :: Double,
    stage_specific_impulse :: Double,
    maneuvers :: [ManeuverEvaluation e]
}

data ManeuverEvaluation e = ManeuverEvaluation {
    maneuver :: Maneuver e,
    maneuver_gravity_delta_v :: Double,
    maneuver_burn_time :: Double,
    maneuver_isp :: Double,
    maneuver_mass_flow :: Double,
    maneuver_initial_mass :: Double,
    maneuver_final_mass :: Double,
    maneuver_ejected_mass :: Double
}

{-
Compensation for gravity
    compute the burn time of the rocket.
    multiply the burn time by the acceleration due to gravity (in the direction opposite to the rocket's thrust),
    subtract the product from the delta-v

    delta_v = isp * log((final_mass + ejected_mass)/final_mass) - g * ejected_mass/mass_flow
-}

{-
if a maneuver requires more delta-v than the stage provides
    reduce the delta-v requirement of the maneuver
    calculuate a new payload
    add another stage
-}

{-
if a manuever requires less delta-v than the stage provides
    compute the ejected mass consumed by the maneuver. solve (numerically) for ejected_mass
    
        isp * log((final_mass + ejected_mass)/final_mass) - g * ejected_mass/mass_flow - delta_v = 0

    compute the delta-v using 
    increase the final mass by the ejected mass when calculating the delta-v for subsequent maneuvers using this stage
    the stage delta-v is the sum of the completed delta-vs for all maneuvers the stage is used for
-}

after :: [Maneuver e] -> Double -> [Maneuver e]
after ms dv | dv <= 0 = ms
after [] _ = []
after (maneuver : ms) dv = 
    if dv >= delta_v maneuver
    then after ms (dv - delta_v maneuver)
    else maneuver {delta_v = delta_v maneuver - dv} : ms

stage_evaluation :: Stage e -> [ManeuverEvaluation e] -> StageEvaluation e
stage_evaluation s ms = StageEvaluation {
    stage_delta_v          = stage_delta_v,
    stage_gravity_delta_v  = sum . map maneuver_gravity_delta_v $ ms,
    stage_burn_time        = sum . map maneuver_burn_time $ ms,
    stage_specific_impulse = stage_delta_v / log ((fromRational . initial_mass) s/(fromRational . final_mass) s),
    maneuvers = ms
}
    where
        stage_delta_v = sum . map (delta_v . maneuver) $ ms

evaluate_stage :: [Maneuver e] -> Stage e -> StageEvaluation e
evaluate_stage maneuvers stage = stage_evaluation stage $ go maneuvers 0
    where
        go [] _ = []
        go (maneuver : ms) ejected_mass =
            if available_delta_v <= delta_v maneuver
            then [ManeuverEvaluation {
                    maneuver = maneuver {delta_v = dv available_ejected_mass},
                    maneuver_gravity_delta_v = gravity_dv available_ejected_mass,
                    maneuver_burn_time       = burn_time  available_ejected_mass,
                    maneuver_isp          = maneuver_isp,
                    maneuver_mass_flow    = maneuver_mass_flow,
                    maneuver_initial_mass = maneuver_initial_mass,
                    maneuver_final_mass   = available_final_mass,
                    maneuver_ejected_mass = available_ejected_mass
                }]
            else ManeuverEvaluation {
                    maneuver = maneuver,
                    maneuver_gravity_delta_v = gravity_dv maneuver_ejected_mass,
                    maneuver_burn_time       = burn_time  maneuver_ejected_mass,
                    maneuver_isp          = maneuver_isp,
                    maneuver_mass_flow    = maneuver_mass_flow,
                    maneuver_initial_mass = maneuver_initial_mass,
                    maneuver_final_mass   = maneuver_initial_mass + maneuver_ejected_mass,
                    maneuver_ejected_mass = maneuver_ejected_mass
                } : go ms (ejected_mass + maneuver_ejected_mass)
            where
                env = environment maneuver
                maneuver_isp = isp stage env
                maneuver_mass_flow = mass_flow stage env
                maneuver_initial_mass = (fromRational . initial_mass) stage + ejected_mass
                available_final_mass = (fromRational . final_mass) stage
                available_ejected_mass = available_final_mass - maneuver_initial_mass

                burn_time :: (Scalar t ~ Double, Mode t, Floating t) => t -> t
                burn_time em = em/auto maneuver_mass_flow

                gravity_dv :: (Scalar t ~ Double, Mode t, Floating t) => t -> t
                gravity_dv em = (auto $ gravity maneuver)*burn_time em

                dv :: (Scalar t ~ Double, Mode t, Floating t) => t -> t
                dv em = auto maneuver_isp * log ((auto maneuver_initial_mass+em)/auto maneuver_initial_mass) - gravity_dv em

                available_delta_v = dv available_ejected_mass
                inv_dv =  last . take 64 . inverse dv 0
                maneuver_ejected_mass = inv_dv (delta_v maneuver)
