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

{-
data StageEvaluation e = SE {
    overall_delta_v :: Double,
    overall_gravity_delta_v :: Double,
    overall_burn_time :: Double,
    stage_specific_impulse :: Double,
    maneuvers :: [ManeuverEvaluation e]
}

data ManeuverEvaluation e = ME {
    maneuver :: Maneuver e,
    delta_v :: Double,
    gravity_delta_v :: Double,
    burn_time :: Double,
    isp :: Double,
    mass_flow :: Double,
    initial_mass :: Double,
    final_mass :: Double,
    ejected_mass :: Double
}
-}

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

evaluate_stage :: [Maneuver e] -> Stage e -> Double
evaluate_stage maneuvers stage = go maneuvers 0
    where
        go [] _ = 0
        go (maneuver : ms) ejected_mass =
            if available_delta_v <= delta_v maneuver
            then available_delta_v
            else delta_v maneuver + go ms (ejected_mass + maneuver_ejected_mass)
            where
                env = environment maneuver
                maneuver_isp = isp stage env
                maneuver_mass_flow = mass_flow stage env
                maneuver_initial_mass = (fromRational . initial_mass) stage + ejected_mass
                available_final_mass = (fromRational . final_mass) stage
                dv :: (Scalar t ~ Double, Mode t, Floating t) => t -> t
                dv = \em -> auto maneuver_isp * log ((auto maneuver_initial_mass+em)/auto maneuver_initial_mass) - (auto $ gravity maneuver)*em/auto maneuver_mass_flow
                available_delta_v = dv (available_final_mass - maneuver_initial_mass)
                em_dv =  last . take 64 . inverse dv 0
                maneuver_ejected_mass = em_dv (delta_v maneuver)
