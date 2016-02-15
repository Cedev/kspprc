module KSP.Data.Environment where

data Environment v = Environment {
    atmospheric_pressure :: v
} deriving (Eq, Show, Read)

space :: Num v => Environment v
space = Environment {
    atmospheric_pressure = 0
}
