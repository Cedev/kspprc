module Data.List.Decorate where
   
increasingOn :: Ord b => (a -> b) -> [a] -> [a]
increasingOn f (x : xs) = x : go (f x) xs
    where
        go best (x : xs) = 
            if f x > best
            then x : go (f x) xs
            else go best xs
        go _ [] = []
increasingOn f [] = []

mergeOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeOn f = go
    where 
        go [] ys = ys
        go xs [] = xs
        go ax@(x:xs) ay@(y:ys)
            | f x <= f y = x : go xs ay
            | otherwise  = y : go ax ys
