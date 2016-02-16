{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module KSP.Data.Resources where

import Data.Foldable
import Data.Traversable

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
} deriving (Eq, Show, Read, Ord, Functor, Foldable, Traversable)

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