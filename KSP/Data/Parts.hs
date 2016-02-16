module KSP.Data.Parts where

import Data.Maybe

import Math.Linear
import Math.Interpolation

import KSP.Data.Bodies
import KSP.Data.Resources

data Part v = Part {
    name :: String,
    dry_cost :: v, -- funds
    dry_mass :: v, -- tons
    capacity :: ResourceVector v, -- units
    geometry :: Geometry,
    thruster :: Maybe (Thruster v),
    decouples :: Bool
} deriving (Eq, Ord, Show, Read)

-- The nothing part
part :: Num v => Part v
part = Part {
    name = [],
    dry_cost = 0,
    dry_mass = 0,
    capacity = zero_resource,
    geometry = symetric [] [],
    thruster = Nothing,
    decouples = False
}

-- propellants
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


-- isp_current = isp_vac - min(p,1) * (isp_vac - isp_atm) where p = pressure in atmospheres
-- https://www.reddit.com/r/KerbalAcademy/comments/1mmuyz/specific_impulse_and_atmospheric_cutoff/

data Thruster v = Thruster {
    vac_thrust :: v, -- kN
    propellant :: ResourceVector v, -- units (ratio)
    isp :: PiecewiseLinear v, -- atmospheric pressure -> isp in seconds
    throttleable :: Bool,
    gimbal :: v,
    mass_flow :: v -- kN / (m/s) = ton / s
} deriving (Eq, Ord, Show, Read)

vac_isp :: (Ord v, Fractional v) => Thruster v -> v
vac_isp = ($ 0) . piecewise_linear . isp

-- max_flow (kg/s) = max_thrust / (g * isp)

vac_flow :: (Ord v, Fractional v) => v -> PiecewiseLinear v -> v
vac_flow vac_thrust isp = vac_thrust/(g * piecewise_linear isp 0)

instance (Ord v, Fractional v) => Monoid (Thruster v) where
    mempty = Thruster {
        vac_thrust = 0,
        propellant = zero_resource,
        isp = [],
        throttleable = False,
        gimbal = 0,
        mass_flow = 0
    }

    Thruster {mass_flow=0} `mappend` y = y
    x `mappend` Thruster {mass_flow=0} = x
    x `mappend` y = Thruster {
        vac_thrust = total_thrust,
        propellant = (+) <$> ((x_ratio*) <$> propellant x) <*> ((y_ratio*) <$> propellant y),
        isp = x_ratio *~ isp x ~+~ y_ratio *~ isp y,
        throttleable = throttleable x && throttleable y,
        gimbal = (vac_thrust x * gimbal x + vac_thrust y * gimbal y)/total_thrust, -- only an approximation
        mass_flow = total_mass_flow
    }
        where
            total_thrust = vac_thrust x + vac_thrust y
            total_mass_flow = mass_flow x + mass_flow y
            x_ratio = mass_flow x/total_mass_flow
            y_ratio = mass_flow y/total_mass_flow

thruster_cluster :: Num v => v -> Thruster v -> Thruster v
thruster_cluster n t = t {
    vac_thrust = n * vac_thrust t,
    mass_flow  = n * mass_flow t
}

data Size = Tiny | Small | Mk1 | Mk2 | Large | Mk3 | ExtraLarge deriving (Eq, Ord, Show, Read)

data Geometry = Geometry {
    asymetrical :: Bool,
    radial_mount :: Bool,
    top :: [Size],
    bottom :: [Size]
} deriving (Eq, Ord, Show, Read)

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
tank name geom cost mass capacity = part {
    name = name,
    dry_cost = cost - capacity `dot` resource_cost,
    dry_mass = mass,
    capacity = capacity,
    geometry = geom
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

liquid_booster :: (Ord v, Fractional v) => String -> Geometry -> v -> v -> v -> PiecewiseLinear v -> v -> ResourceVector v -> Part v
liquid_booster name geom cost mass vac_thrust isp gimbal capacity = part {
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
        gimbal = gimbal,
        mass_flow = vac_flow vac_thrust isp
    })
}

liquid_engine :: (Ord v, Fractional v) => String -> Geometry -> v -> v -> v -> PiecewiseLinear v -> v -> Part v
liquid_engine name geom cost mass vac_thrust isp gimbal = liquid_booster name geom cost mass vac_thrust isp gimbal zero_resource

solid_booster :: (Ord v, Fractional v) => String -> Geometry -> v -> v -> v -> PiecewiseLinear v -> v -> Part v
solid_booster name geom cost mass vac_thrust isp fuel = part {
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
        gimbal = 0,
        mass_flow = vac_flow vac_thrust isp
    })
}
    where capacity = zero_resource { solid_fuel = fuel }

precalculate :: (Ord v, Fractional v) => v -> ResourceVector v -> PiecewiseLinear v -> Bool -> v -> Maybe (Thruster v)
precalculate vac_thrust propellant isp throttleable gimbal = Just $ Thruster {
    vac_thrust = vac_thrust,
    propellant = propellant,
    isp = isp,
    throttleable = throttleable,
    gimbal = gimbal,
    mass_flow = vac_flow vac_thrust isp
}

decoupler :: Num v => String -> Geometry -> v -> v -> Part v
decoupler name geom cost mass = part {
    name = name,
    dry_cost = cost,
    dry_mass = mass,
    geometry = geom,
    decouples = True
}

parts :: (Ord v, Fractional v) => [Part v]
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
    part {
        name = "O-10 \"Puff\" MonoPropellant Fuel Engine",
        dry_cost = 150,
        dry_mass = 0.09,
        capacity = zero_resource,
        geometry = radial,
        thruster = precalculate 20 rcs [(0, 250), (1, 120), (4, 0.001)] True 0
    },
    
    -- lv-n
    part {
        name = "LV-N \"Nerv\" Atomic Rocket Motor",
        dry_cost = 10000,
        dry_mass = 3,
        capacity = zero_resource,
        geometry = small,
        thruster = precalculate 60 lf [(0, 800), (1, 185), (2, 0.001)] True 0
    },
    
    -- dawn
    part {
        name = "IX-6315 \"Dawn\" Electric Propulsion System",
        dry_cost = 8000,
        dry_mass = 0.25,
        capacity = zero_resource,
        geometry = tiny,
        thruster = precalculate 2 xe [(0, 4200), (1, 100), (1.2, 0.001)] True 0
    },
    
    -- solid boosters
    solid_booster "Sepratron I"                                 radial   75 0.01  18 [(0, 154), (1, 118), (6, 0.001)]    8,
    solid_booster "RT-5 \"Flea\" Solid Fuel Booster"             small  200 0.45 192 [(0, 165), (1, 140), (6, 0.001)]  140,
    solid_booster "RT-10 \"Hammer\" Solid Fuel Booster"          small  400 0.75 227 [(0, 195), (1, 170), (7, 0.001)]  375,
    solid_booster "BACC \"Thumper\" Solid Fuel Booster"          small  850 1.5  300 [(0, 210), (1, 175), (6, 0.001)]  820,
    solid_booster "S1 SRB-KD25k \"Kickback\" Solid Fuel Booster" small 2700 4.5  670 [(0, 220), (1, 195), (7, 0.001)] 2600,
    solid_booster "Launch Escape System"         (symetric [] [Small]) 1000 0.9  750 [(0, 180), (1, 160), (8, 0.001)]   30,
    
    -- decouplers
    decoupler "TT-38K Radial Decoupler"       radial 600 0.025,
    decoupler "TT-70 Radial Decoupler"        radial 700 0.05,
    decoupler "Hydraulic Detachment Manifold" radial 770 0.4,
    decoupler "Small Hardpoint"               radial  60 0.05,
    decoupler "Structural Pylon"              radial 125 0.2,
    decoupler "TR-2V Stack Decoupler"           tiny 300 0.015,
    decoupler "TR-2C Stack Separator"           tiny 450 0.02,
    decoupler "TR-18A Stack Decoupler"         small 400 0.05,
    decoupler "TR-18D Stack Separator"         small 600 0.075,
    decoupler "Rockomax Brand Decoupler"       large 550 0.4,
    decoupler "TR-XL Stack Separator"          large 900 0.45,
    decoupler "TR-38-D"                       xlarge 600 0.8
    ]
