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