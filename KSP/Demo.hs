module Main where

import Data.List
import Data.Maybe

import KSP.Stage
import KSP.Data.Bodies
import KSP.Data.Parts

describe_stage s = do
    putStrLn $ "    " ++ (show . fromRational . payload_mass) s ++ " ton payload"
    putStrLn "    -------------"
    
    putStrLn $ "    delta-v    = " ++ show (delta_v s)
    putStrLn $ "    engine-isp = " ++ show (g * (fromRational . engine_isp . engine) s)
    putStrLn $ "    stage-isp  = " ++ show (stage_specific_implulse s)
    putStrLn $ "    total mass = " ++ (show . fromRational . total_mass) s
        
    mapM_ (\g -> putStrLn $ "    " ++ (show . length) g ++ " " ++ head g) . group . map name $ components s
    
    putStrLn ""
    
describe_engine engine = do
    putStrLn . name $ engine
    -- let tanks = filter (fuels (fromJust $ thruster engine)) parts
    -- mapM_ (putStrLn . ("  "++) . name) tanks
    -- putStrLn ""
    
    let stage = optimal_engine_stage 0.1 engine parts
    describe_stage stage


main = do
    let engines = filter (isJust . thruster) parts
    mapM_ describe_engine engines
    
    let mission = optimal_stages 0.1 8000 . filter (radial_mount . geometry) $ parts
    mapM_ describe_stage mission
