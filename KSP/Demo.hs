module Main where

import Data.List
import Data.Maybe

import Math.Interpolation

import qualified Engineering.Rocketry as Rocketry

import KSP.Stage
import KSP.Data.Bodies
import KSP.Data.Environment
import KSP.Data.Parts


describe_stage e = do
    let s = evaluated_stage e

    putStrLn $ "    " ++ (show . fromRational . payload_mass) s ++ " ton payload"
    putStrLn "    -------------"
    
    putStrLn $ "    delta-v    = " ++ show (delta_v e) ++ " m/s"
    putStrLn $ "    engine-isp = " ++ show (g * (fromRational . engine_isp . engine) s) ++ " m/s"
    putStrLn $ "    stage-isp  = " ++ show (stage_specific_implulse e) ++ " m/s"
    putStrLn $ "    total mass = " ++ (show . fromRational . total_mass) s ++ " ton"
    putStrLn $ "    dry mass   = " ++ (show . fromRational . total_dry_mass) s ++ " ton"
    putStrLn $ "    mass flow  = " ++ (show . fromRational . (*1000) . mass_flow) s ++ " kg/s"
    putStrLn $ "    burn time  = " ++ (show . fromRational) ((total_mass s - total_dry_mass s)/mass_flow s) ++ " s"
        
    mapM_ (\g -> putStrLn $ "    " ++ (show . length) g ++ " " ++ head g) . group . map name $ components s
    
    putStrLn ""
    
describe_engine engine = do
    putStrLn . name $ engine
    -- let tanks = filter (fuels (fromJust $ thruster engine)) parts
    -- mapM_ (putStrLn . ("  "++) . name) tanks
    -- putStrLn ""
    
    let stage = optimal_engine_stage stage_delta_v parts 0.1 engine
    describe_stage stage


main = do
    let engines = filter (isJust . thruster) parts
    mapM_ describe_engine engines
    
    putStrLn "Demo mission"
    let mission_profile = [
            Rocketry.Maneuver {Rocketry.delta_v = 6000, Rocketry.gravity=0, Rocketry.environment=space},
            Rocketry.Maneuver {Rocketry.delta_v = 4000, Rocketry.gravity=g, Rocketry.environment=Environment 1}]
        dv = Rocketry.stage_delta_v . Rocketry.evaluate_stage mission_profile . convert_stage
        mission_parts = filter (radial_mount . geometry) parts
        mission = optimal_stages dv parts 0.1 20000
        mission' = optimal_mission_stages parts 0.1 mission_profile
    mapM_ describe_stage mission'
