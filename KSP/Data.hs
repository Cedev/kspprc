{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

import Prelude hiding (sum, mapM_)

import Data.List
import Data.Maybe
import Data.Ord

import Data.Foldable
import Data.Traversable

import Control.Applicative
import Control.Arrow

dot :: (Num a, Foldable f, Applicative f) => f a -> f a -> a
dot x y = sum $ (*) <$> x <*> y

proj :: (Fractional a, Foldable f, Applicative f) => f a -> f a -> f a
proj s v = ((v `dot` s/s `dot` s)*) <$> s

towards :: (Fractional a, Foldable f, Applicative f, Eq (f a)) => f a -> f a -> Bool
towards s v = v /= pure 0 && proj s v == v

g :: Fractional v => v
g = 9.81

data ResourceVector v = ResourceVector {
    electric_charge :: v,
    liquid_fuel :: v,
    oxidizer :: v,
    intake_air :: v,
    solid_fuel :: v,
    monopropellant :: v,
    xenon :: v,
    ore :: v,
    ablator :: v
} deriving (Eq, Show, Read, Functor, Foldable, Traversable)

instance Applicative ResourceVector where
    pure a = ResourceVector a a a a a a a a a
    fs <*> xs = ResourceVector {
        electric_charge = electric_charge fs $ electric_charge xs,
        liquid_fuel     = liquid_fuel     fs $ liquid_fuel     xs,
        oxidizer        = oxidizer        fs $ oxidizer        xs,
        intake_air      = intake_air      fs $ intake_air      xs,
        solid_fuel      = solid_fuel      fs $ solid_fuel      xs,
        monopropellant  = monopropellant  fs $ monopropellant  xs,
        xenon           = xenon           fs $ xenon           xs,
        ore             = ore             fs $ ore             xs,
        ablator         = ablator         fs $ ablator         xs
    }

zero_resource :: Num a => ResourceVector a
zero_resource = pure 0

resource_density :: Fractional a => ResourceVector a -- tons/unit
resource_density = (/1000) <$> ResourceVector {
    electric_charge = 0,
    liquid_fuel = 5, -- kg/unit
    oxidizer = 5,
    intake_air = 5,
    solid_fuel = 7.5,
    monopropellant = 4,
    xenon = 0.1,
    ore = 10,
    ablator = 1
}

resource_cost :: Fractional a => ResourceVector a -- funds/unit
resource_cost = ResourceVector {
    electric_charge = 0,
    liquid_fuel = 0.8,
    oxidizer = 0.18,
    intake_air = 0,
    solid_fuel = 0.6,
    monopropellant = 1.2,
    xenon = 4,
    ore = 0.02,
    ablator = 0.5
}

data Part v = Part {
    name :: String,
    dry_cost :: v, -- funds
    dry_mass :: v, -- tons
    capacity :: ResourceVector v, -- units
    geometry :: Geometry,
    thruster :: Maybe (Thruster v)
} deriving (Eq, Show, Read)

lfo :: Fractional a => ResourceVector a
lfo = zero_resource {liquid_fuel = 0.9, oxidizer = 1.1}

lf :: Fractional a => ResourceVector a
lf = zero_resource {liquid_fuel = 1}

sf :: Fractional a => ResourceVector a
sf = zero_resource {solid_fuel = 1}

xe :: Fractional a => ResourceVector a
xe = zero_resource {xenon = 0.1, electric_charge = 18}

rcs :: Fractional a => ResourceVector a
rcs = zero_resource {monopropellant = 1}

type PiecewiseLinear v = [(v, v)]

-- isp_current = isp_vac - min(p,1) * (isp_vac - isp_atm) where p = pressure in atmospheres
-- https://www.reddit.com/r/KerbalAcademy/comments/1mmuyz/specific_impulse_and_atmospheric_cutoff/


data Thruster v = Thruster {
    vac_thrust :: v, -- kN
    propellant :: ResourceVector v, -- units (ratio)
    isp :: PiecewiseLinear v, -- atmospheric pressure -> isp in seconds
    throttleable :: Bool,
    gimbal :: v
} deriving (Eq, Show, Read)

-- max_flow (kg/s) = max_thrust / (g * isp)

data Size = Tiny | Small | Mk1 | Mk2 | Large | Mk3 | ExtraLarge deriving (Eq, Ord, Show, Read)

data Geometry = Geometry {
    asymetrical :: Bool,
    radial_mount :: Bool,
    top :: [Size],
    bottom :: [Size]
} deriving (Eq, Show, Read)

symetric :: [Size] -> [Size] -> Geometry
symetric top bottom = Geometry {
    asymetrical = False,
    radial_mount = False,
    top = top,
    bottom = bottom
}

cylinder :: Size -> Geometry
cylinder size = symetric [size] [size]

tiny   = cylinder Tiny
small  = cylinder Small
large  = cylinder Large
xlarge = cylinder ExtraLarge
mk1 = cylinder Mk1
mk2 = cylinder Mk2
mk3 = cylinder Mk3

radial :: Geometry
radial = Geometry {
    asymetrical = True,
    radial_mount = True,
    top = [],
    bottom = []
}

conic :: Size -> Size -> Geometry
conic top bottom = symetric [top] [bottom]

aconic :: Size -> Size -> Geometry
aconic top bottom = (conic top bottom) {asymetrical = True}

tank :: Fractional v => String -> Geometry -> v -> v -> ResourceVector v -> Part v
tank name geom cost mass capacity = Part {
    name = name,
    dry_cost = cost - capacity `dot` resource_cost,
    dry_mass = mass,
    capacity = capacity,
    geometry = geom,
    thruster = Nothing
}

rocket_tank :: Fractional v => String -> Geometry -> v -> v -> v -> v -> Part v
rocket_tank name geom cost mass fuel oxidizer = tank name geom cost mass $ zero_resource {liquid_fuel = fuel, oxidizer = oxidizer}

liquid_tank :: Fractional v => String -> Geometry -> v -> v -> v -> Part v
liquid_tank name geom cost mass fuel = rocket_tank name geom cost mass fuel 0

rcs_tank :: Fractional v => String -> Geometry -> v -> v -> v -> Part v
rcs_tank name geom cost mass capacity = tank name geom cost mass $ zero_resource {monopropellant = capacity}

xenon_tank :: Fractional v => String -> Geometry -> v -> v -> v -> Part v
xenon_tank name geom cost mass capacity = tank name geom cost mass $ zero_resource {xenon = capacity}

ore_tank :: Fractional v => String -> Geometry -> v -> v -> v -> Part v
ore_tank name geom cost mass capacity = tank name geom cost mass $ zero_resource {ore = capacity}

battery :: Fractional v => String -> Geometry -> v -> v -> v -> Part v
battery name geom cost mass capacity = tank name geom cost mass $ zero_resource {electric_charge = capacity}

liquid_booster :: Fractional v => String -> Geometry -> v -> v -> v -> PiecewiseLinear v -> v -> ResourceVector v -> Part v
liquid_booster name geom cost mass vac_thrust isp gimbal capacity = Part {
    name = name,
    dry_cost = cost - capacity `dot` resource_cost,
    dry_mass = mass,
    capacity = capacity,
    geometry = geom,
    thruster = Just (Thruster {
        vac_thrust = vac_thrust,
        propellant = lfo,
        isp = isp,
        throttleable = True,
        gimbal = gimbal
    })
}

liquid_engine :: Fractional v => String -> Geometry -> v -> v -> v -> PiecewiseLinear v -> v -> Part v
liquid_engine name geom cost mass vac_thrust isp gimbal = liquid_booster name geom cost mass vac_thrust isp gimbal zero_resource

solid_booster :: Fractional v => String -> Geometry -> v -> v -> v -> PiecewiseLinear v -> v -> Part v
solid_booster name geom cost mass vac_thrust isp fuel = Part {
    name = name,
    dry_cost = cost - capacity `dot` resource_cost,
    dry_mass = mass,
    capacity = capacity,
    geometry = geom,
    thruster = Just (Thruster {
        vac_thrust = vac_thrust,
        propellant = sf,
        isp = isp,
        throttleable = False,
        gimbal = 0
    })
}
    where capacity = zero_resource { solid_fuel = fuel }

parts :: Fractional v => [Part v]
parts = [
    -- rocket fuel tanks
    rocket_tank "Oscar-B Fuel Tank"             tiny    70 0.025    18   22,
    rocket_tank "ROUND-8 Toroidal Fuel Tank"    tiny   175 0.0375   27   33,
    rocket_tank "FL-T100 Fuel Tank"            small   150 0.0625   45   55,
    rocket_tank "FL-T200 Fuel Tank"            small   275 0.125    90  110,
    rocket_tank "FL-T400 Fuel Tank"            small   500 0.25    180  220,
    rocket_tank "FL-T800 Fuel Tank"            small   800 0.5     360  440,
    rocket_tank "Rockomax X200-8 Fuel Tank"    large   800 0.5     360  440,
    rocket_tank "Rockomax X200-16 Fuel Tank"   large  1550 1       720  880,
    rocket_tank "Rockomax X200-32 Fuel Tank"   large  3000 2      1440 1760,
    rocket_tank "Rockomax Jumbo-64 Fuel Tank"  large  5750 4      2880 3520,
    rocket_tank "Kerbodyne S3-3600 Tank"      xlarge  3250 2.25   1620 1980,
    rocket_tank "Kerbodyne S3-7200 Tank"      xlarge  6500 4.5    3240 3960,
    rocket_tank "Kerbodyne S3-14400 Tank"     xlarge 13000 9      6480 7920,
    
    -- fuselages
    rocket_tank "Mk2 Rocket Fuel Fuselage Short"   mk2   750 0.29  180  220,
    rocket_tank "Mk2 Rocket Fuel Fuselage"         mk2  1450 0.57  360  440,
    rocket_tank "Mk3 Rocket Fuel Fuselage Short"   mk3  4300 1.79 1125 1375,
    rocket_tank "Mk3 Rocket Fuel Fuselage"         mk3  8600 3.57 2250 2750,
    rocket_tank "Mk3 Rocket Fuel Fuselage Long"    mk3 17200 7.14 4500 5500,
    
    -- adapters
    rocket_tank "C7 Brand Adapter - 2.5m to 1.25m"         ( conic Small Large)  800 0.57  360  440,
    rocket_tank "C7 Brand Adapter Slanted - 2.5m to 1.25m" (aconic Small Large)  800 0.57  360  440,
    rocket_tank "Mk2 to 1.25m Adapter"                     ( conic Small   Mk2)  550 0.29  180  220,
    rocket_tank "Mk2 to 1.25m Adapter Long"                ( conic Small   Mk2) 1050 0.57  360  440,
    rocket_tank "2.5m to Mk2 Adapter"                      ( conic   Mk2 Large)  800 0.57  360  440,
    rocket_tank "Mk3 to Mk2 Adapter"                       (aconic   Mk2   Mk3) 2200 1.43  900 1100,
    rocket_tank "Mk3 to 2.5m Adapter"                      ( conic Large   Mk3) 2500 1.79 1125 1375,
    rocket_tank "Mk3 to 2.5m Adapter Slanted"              (aconic Large   Mk3) 2500 1.79 1125 1375,
    rocket_tank "Mk2 Bicoupler"                 (symetric [Mk2] [Small, Small])  860 0.29  180  220,
    
    -- liquid fuel tanks
    liquid_tank "Mk0 Liquid Fuel Fuselage"        tiny   200 0.03    50,
    liquid_tank "Mk1 Liquid Fuel Fuselage"       small   550 0.25   400,
    liquid_tank "Mk2 Liquid Fuel Fuselage Short"   mk2   750 0.29   400,
    liquid_tank "Mk2 Liquid Fuel Fuselage"         mk2  1450 0.57   800,
    liquid_tank "Mk3 Liquid Fuel Fuselage Short"   mk3  4300 1.79  2500,
    liquid_tank "Mk3 Liquid Fuel Fuselage"         mk3  8600 3.57  5000,
    liquid_tank "Mk3 Liquid Fuel Fuselage Long"    mk3 17200 7.14 10000,
    -- adapters
    liquid_tank "NCS Adapter"       (conic Tiny Small)   320 0.1     80,
    -- wings
    liquid_tank "Big-S Wing Strake"             radial  1000 0.1    100,
    liquid_tank "Big-S Delta Wing"              radial  3000 0.5    300,
    liquid_tank "FAT-455 Aeroplane Main Wing"   radial  2800 0.78   600,
    
    -- combination intakes
    
    -- monopropellant tanks
    rcs_tank "FL-R10 RCS Fuel Tank"     tiny  200 0.05   80,
    rcs_tank "FL-R25 RCS Fuel Tank"    small  600 0.15  250,
    rcs_tank "FL-R1 RCS Fuel Tank"     large 1300 0.4   750,
    rcs_tank "Mk2 Monopropellant Tank"   mk2  750 0.29  400,
    rcs_tank "Mk3 Monopropellant Tank"   mk3 4300 0.71 1000,
    rcs_tank "Stratus-V Roundified Monopropellant Tank"   radial 200 0.075  60,
    rcs_tank "Stratus-V Cylindrified Monopropellant Tank" radial 450 0.15  150,
    
    -- xenon tanks
    xenon_tank "PB-X50R Xenon Container"  radial  2200 0.03  400,
    xenon_tank "PB-X150R Xenon Container"   tiny  3000 0.05  700,
    xenon_tank "PB-X250R Xenon Container"  small 22500 0.41 5250,
    
    -- ore tanks
    ore_tank "Radial Holding Tank" radial  300 0.125   75,
    ore_tank "Small Holding Tank"   small 1000 0.5    300,
    ore_tank "Large Holding Tank"   large 3000 2     1500,
    
    -- batteries
    battery "Z-100 Rechargeable Battery Pack" radial   80 0.005  100,
    battery "Z-200 Rechargeable Battery Bank"   tiny  360 0.01   200,
    battery "Z-400 Rechargeable Battery"      radial  550 0.02   400,
    battery "Z-1k Rechargeable Battery Bank"   small  880 0.05  1000,
    battery "Z-4K Rechargeable Battery Bank"   large 4500 0.2   4000,
    
    -- liquid engines
    liquid_engine "LV-1R \"Spider\" Liquid Fuel Engine"   radial   120 0.02    2 [(0,290), (1,260), ( 8,0.001)]  8,
    liquid_engine "24-77 \"Twitch\" Liquid Fuel Engine"   radial   400 0.09   16 [(0,290), (1,250), ( 7,0.001)]  8,
    liquid_engine "Mk-55 \"Thud\" Liquid Fuel Engine"     radial   820 0.9   120 [(0,305), (1,275), ( 9,0.001)]  8,
    liquid_engine "LV-1 \"Ant\" Liquid Fuel Engine"         tiny   110 0.02    2 [(0,315), (1, 80), ( 3,0.001)]  0,
    liquid_engine "48-7s \"Spark\" Liquid Fuel Engine"      tiny   200 0.1    18 [(0,300), (1,270), ( 7,0.001)]  3,
    liquid_engine "LV-909 \"Terrier\" Liquid Fuel Engine"  small   390 0.5    60 [(0,345), (1, 80), ( 3,0.001)]  4,
    liquid_engine "LV-30 \"Reliant\" Liquid Fuel Engine"   small  1100 1.25  215 [(0,300), (1,280), ( 7,0.001)]  0,
    liquid_engine "LV-45 \"Swivel\" Liquid Fuel Engine"    small  1200 1.5   200 [(0,320), (1,270), ( 6,0.001)]  3,
    liquid_engine "S3 KS-25 \"Vector\" Liquid Fuel Engine" small 18000 4    1000 [(0,315), (1,295), (12,0.001)] 10.5,
    liquid_engine "RE-L10 \"Poodle\" Liquid Fuel Engine"   large  1300 1.75  250 [(0,350), (1, 90), ( 3,0.001)]  4.5,
    liquid_engine "RE-I5 \"Skipper\" Liquid Fuel Engine"   large  5300 3     650 [(0,320), (1,280), ( 6,0.001)]  2,
    liquid_engine "RE-M3 \"Mainsail\" Liquid Fuel Engine"  large 13000 6    1500 [(0,310), (1,285), ( 9,0.001)]  2,
    
    liquid_engine "T-1 Toroidal Aerospike \"Dart\" Liquid Fuel Engine" small 3850 1 180 [(0,340), (1,290), (5,230), (10,170), (20,0.001)] 0,
    
    liquid_engine "Kerbodyne KR-2L+ \"Rhino\" Liquid Fuel Engine"                     xlarge 25000  9 2000 [(0,340), (1,255), ( 5,0.001)] 4,
    liquid_engine "S3 KS-25x4 \"Mammoth\" Liquid Fuel Engine"     (symetric [ExtraLarge] []) 39000 15 4000 [(0,315), (1,295), (12,0.001)] 2,

    liquid_engine "CR-7 R.A.P.I.E.R. Engine"               small  6000 2     180 [(0,305), (1,275), ( 9,0.001)]  3,

    liquid_booster "LFB KR-1x2 \"Twin-Boar\" Liquid Fuel Engine" (symetric [Large] []) 17000 10.5 2000 [(0,300), (1,280), (9,0.001)] 1.5 $ zero_resource {
        liquid_fuel = 2880,
        oxidizer    = 3520
        },
    
    -- puff
    Part {
        name = "O-10 \"Puff\" MonoPropellant Fuel Engine",
        dry_cost = 150,
        dry_mass = 0.09,
        capacity = zero_resource,
        geometry = radial,
        thruster = Just (Thruster {
            vac_thrust = 20,
            propellant = rcs,
            isp = [(0, 250), (1, 120), (4, 0.001)],
            throttleable = True,
            gimbal = 0
        })
    },
    
    -- lv-n
    Part {
        name = "LV-N \"Nerv\" Atomic Rocket Motor",
        dry_cost = 10000,
        dry_mass = 3,
        capacity = zero_resource,
        geometry = small,
        thruster = Just (Thruster {
            vac_thrust = 60,
            propellant = lf,
            isp = [(0, 800), (1, 185), (2, 0.001)],
            throttleable = True,
            gimbal = 0
        })
    },
    
    -- dawn
    Part {
        name = "IX-6315 \"Dawn\" Electric Propulsion System",
        dry_cost = 8000,
        dry_mass = 0.25,
        capacity = zero_resource,
        geometry = tiny,
        thruster = Just (Thruster {
            vac_thrust = 2,
            propellant = xe,
            isp = [(0, 4200), (1, 100), (1.2, 0.001)],
            throttleable = True,
            gimbal = 0
        })
    },
    
    -- solid boosters
    solid_booster "Sepratron I"                                 radial   75 0.01  18 [(0, 154), (1, 118), (6, 0.001)]    8,
    solid_booster "RT-5 \"Flea\" Solid Fuel Booster"             small  200 0.45 192 [(0, 165), (1, 140), (6, 0.001)]  140,
    solid_booster "RT-10 \"Hammer\" Solid Fuel Booster"          small  400 0.75 227 [(0, 195), (1, 170), (7, 0.001)]  375,
    solid_booster "BACC \"Thumper\" Solid Fuel Booster"          small  850 1.5  300 [(0, 210), (1, 175), (6, 0.001)]  820,
    solid_booster "S1 SRB-KD25k \"Kickback\" Solid Fuel Booster" small 2700 4.5  670 [(0, 220), (1, 195), (7, 0.001)] 2600,
    solid_booster "Launch Escape System"         (symetric [] [Small]) 1000 0.9  750 [(0, 180), (1, 160), (8, 0.001)]   30
    
    ]
    
fuels :: Thruster Rational -> Part Rational -> Bool
fuels engine part = towards (propellant engine) (capacity part) && isNothing (thruster part)


engine_isp = snd . head . isp

data Stage = Stage {
    engine :: Thruster Rational,
    delta_v :: Double,
    stage_specific_implulse :: Double,
    total_dry_mass :: Rational,
    total_capacity :: ResourceVector Rational,
    total_mass :: Rational,
    total_parts :: Int,
    total_cost :: Rational,
    components :: [Part Rational],
    payload_mass :: Rational
}

stage :: Rational -> Thruster Rational -> [Part Rational] -> Stage
stage payload engine components = Stage {
    engine = engine,
    delta_v = delta_v,
    stage_specific_implulse = stage_specific_implulse,
    total_dry_mass = total_dry_mass,
    total_capacity = total_capacity,
    total_mass = total_mass,
    total_parts = total_parts,
    total_cost = total_cost,
    components = components,
    payload_mass = payload
}
    where
        delta_v = log (fromRational total_mass/fromRational total_dry_mass) * (fromRational . engine_isp) engine * g
        stage_specific_implulse = delta_v / log (fromRational total_mass / fromRational payload)
        total_dry_mass = payload + (sum . map dry_mass) components
        total_capacity = foldl (liftA2 (+)) zero_resource . map capacity $ components
        total_mass = total_dry_mass + resource_density `dot` total_capacity
        total_parts = length components
        total_cost = resource_cost `dot` total_capacity + (sum . map dry_cost) components

extend :: Part Rational -> Stage -> Stage
extend p s = s' where s' = Stage {
    engine = engine s,
    delta_v = log ((fromRational . total_mass) s'/(fromRational . total_dry_mass) s') * (fromRational . engine_isp . engine) s' * g,
    stage_specific_implulse = delta_v s' / log ((fromRational . total_mass) s' / (fromRational . payload_mass) s'),
    total_dry_mass = total_dry_mass s + dry_mass p,
    total_capacity = (+) <$> total_capacity s <*> capacity p,
    total_mass = total_mass s + dry_mass p + resource_density `dot` capacity p,
    total_parts = total_parts s + 1,
    total_cost = total_cost s + dry_cost p + resource_cost `dot` capacity p,
    components = p : components s,
    payload_mass = payload_mass s
}

-- If a part's asymetrical we have to add two of them the first time we add one.
extend_symetric :: Part Rational -> Stage -> Stage
extend_symetric p s =
    if (asymetrical . geometry) p && notElem p (components s)
    then (extend p . extend p) s
    else extend p s


stage_metric = stage_specific_implulse &&& Down . total_parts &&& Down . total_cost
        
increasingOn :: Ord b => (a -> b) -> [a] -> [a]
increasingOn f (x : xs) = x : go (f x) xs
    where
        go best (x : xs) = 
            if f x > best
            then x : go (f x) xs
            else go best xs
        go _ [] = []
increasingOn f [] = []
        
optimal_engine_stage :: Rational -> Part Rational -> [Part Rational] -> Stage
optimal_engine_stage payload engine part_db = expand empty_stage parts []
    where
        -- s0 is the new best stage, or it would have been filtered out.
        go :: Stage -> [([Part Rational], Stage)] -> Stage
        go best_stage ((p0, s0) : ss) = expand s0 p0 ss
        go best_stage []              = best_stage
        
        expand :: Stage -> [Part Rational] -> [([Part Rational], Stage)] -> Stage
        expand best_stage parts ss = go best_stage sorted
            where
                increasing = increasingOn (stage_metric . snd) sorted
                sorted = sortOn (expansion_order . snd) filtered
                filtered = filter (\(_, s) -> stage_metric s > stage_metric best_stage) expanded
                expanded = [(p, extend_symetric (head p) best_stage) | p <- (init . tails) parts] ++ ss
                
        expansion_order = total_dry_mass &&& Down . stage_metric

        parts :: [Part Rational]
        parts = sortOn (Down . dry_mass) . filter (fuels stage_thruster) $ part_db
        
        stage_thruster = (fromJust . thruster) engine
        
        empty_stage :: Stage
        empty_stage = extend_symetric engine $ stage payload stage_thruster []

        
optimal_stage :: Rational -> [Part Rational] -> Stage
optimal_stage payload part_db = head . sortOn (Down . stage_metric) $ engine_stages
    where
        engine_stages = [optimal_engine_stage payload engine part_db | engine <- engines]
        engines = filter (isJust . thruster) part_db
        
optimal_stages' :: Rational -> [Part Rational] -> [Stage]
optimal_stages' payload part_db = s0 : optimal_stages' (total_mass s0) part_db
    where
        s0 = optimal_stage payload part_db
        
optimal_stages :: Rational -> Rational -> [Part Rational] -> [Stage]
optimal_stages payload mission_delta_v part_db = head . dropWhile ((< fromRational mission_delta_v) . sum . map delta_v) . inits $ optimal_stages' payload part_db
        
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
