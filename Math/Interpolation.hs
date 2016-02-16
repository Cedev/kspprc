module Math.Interpolation where

import Control.Arrow

import qualified Data.Set as Set
    
type PiecewiseLinear v = [(v, v)]

piecewise_linear :: (Ord v, Fractional v) => PiecewiseLinear v -> (v -> v)
piecewise_linear []             _ = error "piecewise_linear interpolation requires at least one point"
piecewise_linear ((_,  y) : []) _ = y
piecewise_linear ((x0, y) : _)  x
    | x <= x0   = y
piecewise_linear ((x0, y0) : ps@((x1, y1) : _)) x  
    | x <= x1   = y0 + (x - x0) * (y1 - y0)/(x1 - x0)
    | otherwise = piecewise_linear ps x

(*~) :: Num v => v -> PiecewiseLinear v -> PiecewiseLinear v
c *~ f = map (fmap (c*)) f

infixr 7 *~

(~+~) :: (Ord v, Fractional v) => PiecewiseLinear v -> PiecewiseLinear v -> PiecewiseLinear v
f ~+~ g = map (id &&& fg) . Set.elems $ Set.union (Set.fromList . map fst $ f) (Set.fromList . map fst $ g)
    where fg x = piecewise_linear f x + piecewise_linear g x

infixl 6 ~+~
