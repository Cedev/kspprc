module Search where

import Data.List
import Data.List.Decorate

import Control.Arrow

import Control.Monad.Trans.State

visitOn :: (Ord e, Monad m) => (a -> e) ->             -- Order to visit the nodes
                               (a -> m (a -> Bool)) -> -- State change when visiting a node.
                                                       -- Returns which nodes to keep for current state
                               (a -> [a]) ->           -- How to expand a node
                               [a] ->                  -- Initial nodes
                               m ()                    -- Final state
visitOn expansion_order visit expand seeds = go_combine (const True) seeds []
    where
        go_combine keep new = go . filter keep . mergeOn expansion_order (sortOn expansion_order new)

        -- x is the new best, or it would have been filtered out.
        go (x : xs) = do
                          keep <- visit x
                          go_combine keep (expand x) xs
        go []       = return ()

visitOn_ :: Ord e => (a -> e) ->
                     (a -> a -> Bool) ->
                     (a -> [a]) ->
                     a ->
                     a
visitOn_ expansion_order better_than expand initial = execState search initial
    where
        search = visitOn expansion_order visit expand . filter (better_than initial) . expand $ initial
        visit a = put a >> return (better_than a)

-- find maximum of a function that starts increasing from 0 and then decreases.
unimodalMaximumOn :: Ord b => (a -> b) -> (Integer -> a) -> a
unimodalMaximumOn y f = go3 (tag 0) (tag 1) (tag 2)
    where
        -- look for a point where the function starts decreasing

        go3 t0@(x0, y0, a0) t1@(x1, y1, a1) t2@(x2, y2, a2) =
            case (compare y0 y1, compare y1 y2) of
                (LT, LT) -> go3 t1 t2 (tag $ 2 * x2)
                (LT, EQ) -> go_halvsies t1 t2
                (LT, GT) -> 
                    if x1 == x0 + 1 && x2 == x0 + 2
                    then a1
                    else if (x1 - x0) > (x2 - x1)
                         then go4 t0 (tag $ half x0 x1) t1 t2
                         else go4 t0 t1 (tag $ half x1 x2) t2
                (EQ, _) -> go_halvsies t0 t1
                (GT, _) -> go_halvsies t0 t1

        go4 t0@(x0, y0, a0) t1@(x1, y1, a1) t2@(x2, y2, a2) t3@(x3, y3, a3) =
            case (compare y0 y1, compare y1 y2, compare y2 y3) of
                (LT, LT, LT) -> go3 t1 t2 t3      -- wtf
                (LT, LT, EQ) -> go_halvsies t2 t3
                (LT, LT, GT) -> go3 t1 t2 t3
                (LT, EQ, _ ) -> go_halvsies t1 t2
                (LT, GT, _ ) -> go3 t0 t1 t2
                (EQ, _ , _ ) -> go_halvsies t0 t1
                (GT, _ , _ ) -> go_halvsies t0 t1

        go_halvsies t0@(x0, y0, a0) t1@(x1, y1, a1) =
            if x1 == x0 + 1
            then a0
            else go3 t0 (tag $ half x0 x1) t1

        half x0 x1 = x0 + (x1 - x0) `div` 2

        tag x = (x, y a, a) where a = f x
