module Math.Interpolation where
    
type PiecewiseLinear v = [(v, v)]

piecewise_linear :: (Ord v, Fractional v) => PiecewiseLinear v -> (v -> v)
piecewise_linear []             _ = error "piecewise_linear interpolation requires at least one point"
piecewise_linear ((_,  y) : []) _ = y
piecewise_linear ((x0, y) : _)  x
    | x <= x0   = y
piecewise_linear ((x0, y0) : ps@((x1, y1) : _)) x  
    | x <= x1   = y0 + (x - x0) * (y1 - y0)/(x1 - x0)
    | otherwise = piecewise_linear ps x
