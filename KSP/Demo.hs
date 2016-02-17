module Main where

import Data.List
import Data.Maybe

import Math.Interpolation

import qualified Engineering.Rocketry as Rocketry

import KSP.Data.Bodies
import KSP.Data.Environment
import KSP.Data.Parts

import KSP.Assembly
import KSP.Calculator
import KSP.Evaluation
import KSP.Stage

describe_stage es = do
    let s = evaluated_stage es
        e = evaluation es

    putStrLn $ "    " ++ (show . fromRational . payload_mass) s ++ " ton payload"
    putStrLn "    -------------"
    
    putStrLn $ "    delta-v    = " ++ show (Rocketry.stage_delta_v e) ++ " m/s"
    putStrLn $ "    engine-isp = " ++ show (g * (fromRational . vac_isp . engine) s) ++ " m/s"
    putStrLn $ "    stage-isp  = " ++ show (Rocketry.stage_specific_impulse e) ++ " m/s"
    putStrLn $ "    total mass = " ++ (show . fromRational . total_mass) s ++ " ton"
    putStrLn $ "    dry mass   = " ++ (show . fromRational . total_dry_mass) s ++ " ton"
    putStrLn $ "    mass flow  = " ++ (show . fromRational . (*1000) . mass_flow . engine) s ++ " kg/s"
    putStrLn $ "    burn time  = " ++ (show . fromRational) ((total_mass s - total_dry_mass s)/(mass_flow . engine) s) ++ " s"
        
    mapM_ (\(p, n) -> putStrLn $ "    " ++ show n ++ " " ++ name p) . quantities $ components s
    
    putStrLn ""

describe_engine engine = do
    return ()

    {-
    putStrLn . name $ engine
    let tanks = filter (fuels (fromJust $ thruster engine)) parts
    mapM_ (putStrLn . ("  "++) . name) tanks
    putStrLn ""
    -}

main = do
    let engines = filter (isJust . thruster) parts
    mapM_ describe_engine engines
    
    putStrLn "Demo mission"
    let mission_profile = [
            -- Rocketry.Maneuver {Rocketry.delta_v = 6000, Rocketry.gravity=0, Rocketry.environment=space}
            --,
            Rocketry.Maneuver {Rocketry.delta_v = 4000, Rocketry.gravity=g, Rocketry.environment=Environment 1}
            ]
        mission_assembly = mission_vehicle_assembly 100 mission_profile parts
        mission' = shifted_mission_stages 100 mission_assembly
    mapM_ describe_stage mission'
