module Search where

import Data.List
import Data.List.Decorate

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
        search = visitOn expansion_order visit expand (expand initial)
        visit a = put a >> return (better_than a)
