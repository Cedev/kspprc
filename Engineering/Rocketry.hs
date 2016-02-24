{-# LANGUAGE TypeFamilies #-}

module Engineering.Rocketry where

import Data.Ord

import Control.Arrow

import Control.Monad.Trans.State

import Numeric.AD.Mode
import Numeric.AD.Newton.Double

import Search

import Debug.Trace
traceShow_ f x = traceShow (f x) x
traceShow' n f x = trace (n ++ (show . f) x) x

data Stage e = Stage {
    payload_mass :: Rational,
    total_mass :: Rational,
    dry_mass :: Rational,
    isp :: e -> Double,
    mass_flow :: e -> Double
}

data Maneuver e = Maneuver {
    delta_v :: Double,
    gravity :: Double,
    environment :: e
} deriving (Eq, Show, Read)

data StageEvaluation e = StageEvaluation {
    stage_delta_v :: Double,
    stage_gravity_delta_v :: Double,
    stage_burn_time :: Double,
    stage_specific_impulse :: Double,
    stage_payload_mass :: Double,
    stage_total_mass :: Double,
    stage_dry_mass :: Double,
    maneuvers :: [ManeuverEvaluation e]
} deriving (Eq, Show, Read)

data ManeuverEvaluation e = ManeuverEvaluation {
    maneuver :: Maneuver e,
    maneuver_gravity_delta_v :: Double,
    maneuver_burn_time :: Double,
    maneuver_isp :: Double,
    maneuver_mass_flow :: Double,
    maneuver_total_mass :: Double,
    maneuver_dry_mass :: Double,
    maneuver_ejected_mass :: Double
} deriving (Eq, Show, Read)

{-
Compensation for gravity
    compute the burn time of the rocket.

    See notes
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
    
        isp * log((dry_mass + ejected_mass)/dry_mass) - g * ejected_mass/mass_flow - delta_v = 0

    compute the delta-v using 
    increase the final mass by the ejected mass when calculating the delta-v for subsequent maneuvers using this stage
    the stage delta-v is the sum of the completed delta-vs for all maneuvers the stage is used for
-}

infinity = encodeFloat (floatRadix 0 - 1) (snd $ floatRange 0)

forever :: Maneuver e -> Maneuver e
forever m = m {delta_v = infinity}

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
    stage_payload_mass     = stage_payload_mass,
    stage_total_mass       = stage_total_mass,
    stage_dry_mass         = stage_dry_mass,
    stage_specific_impulse = stage_delta_v / log (stage_total_mass/stage_payload_mass),
    maneuvers = ms
}
    where
        stage_payload_mass = fromRational $ payload_mass s
        stage_total_mass = fromRational $ total_mass s
        stage_dry_mass   = fromRational $ dry_mass s
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
                    maneuver_total_mass   = available_total_mass,
                    maneuver_dry_mass     = maneuver_dry_mass,
                    maneuver_ejected_mass = available_ejected_mass
                }]
            else ManeuverEvaluation {
                    maneuver = maneuver,
                    maneuver_gravity_delta_v = gravity_dv maneuver_ejected_mass,
                    maneuver_burn_time       = burn_time  maneuver_ejected_mass,
                    maneuver_isp          = maneuver_isp,
                    maneuver_mass_flow    = maneuver_mass_flow,
                    maneuver_total_mass   = maneuver_dry_mass + maneuver_ejected_mass,
                    maneuver_dry_mass     = maneuver_dry_mass,
                    maneuver_ejected_mass = maneuver_ejected_mass
                } : go ms (ejected_mass + maneuver_ejected_mass)
            where
                env = environment maneuver
                maneuver_isp = isp stage env
                maneuver_mass_flow = mass_flow stage env
                maneuver_dry_mass = (fromRational . dry_mass) stage + ejected_mass
                available_total_mass = (fromRational . total_mass) stage
                available_ejected_mass = available_total_mass - maneuver_dry_mass

                burn_time :: (Scalar t ~ Double, Mode t, Floating t) => t -> t
                burn_time em = em/auto maneuver_mass_flow

                stage_dv ::  (Scalar t ~ Double, Mode t, Floating t) => t -> t
                stage_dv em = auto maneuver_isp * log ((auto maneuver_dry_mass+em)/auto maneuver_dry_mass) 

                -- Assumes constant acceleration
                gravity_dv :: (Scalar t ~ Double, Mode t, Floating t) => t -> t
                gravity_dv em = sqrt(stage_dv em *(auto $ gravity maneuver)*burn_time em + auto (ve*ve)) - auto ve

                -- Assumes the effective final velocity (Ve) of the maneuver is the maneuver delta-v
                ve = delta_v maneuver

                dv :: (Scalar t ~ Double, Mode t, Floating t) => t -> t
                dv em = if maneuver_isp * maneuver_mass_flow >= maneuver_dry_mass * gravity maneuver
                        then stage_dv em - gravity_dv em
                        else stage_dv em - (auto $ gravity maneuver)*burn_time em

                available_delta_v = dv available_ejected_mass
                inv_dv =  last . take 64 . inverse dv 0
                maneuver_ejected_mass = inv_dv (delta_v maneuver)

{- marginal stage specific impulse is
    delta_v now - delta_v before
    ----------------------------
     log (mass now/mass before)

    as long as the marginal stage specific impulse is better than the
    specific impulse of the best possible stage in the current environment
    it's probably better to keep extending the current stage
-}
marginal_stage_specific_impulse :: StageEvaluation e -> StageEvaluation e -> Double
marginal_stage_specific_impulse before now = (stage_delta_v now - stage_delta_v before)/log(stage_total_mass now/stage_total_mass before)
