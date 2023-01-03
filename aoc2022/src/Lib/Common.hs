module Lib.Common (
    toArray,
    Cost(..),
    Q,
    addCost,
    astar,
    dijkstrasArr,
    dijkstras
) where

import Data.Array ( Ix(range), array, bounds, Array )
import qualified Data.Map as M
import qualified Data.Set as S

-- >> toArray [[0,1],[2,3]]
-- array ((0,0),(1,1)) [((0,0),0),((0,1),1),((1,0),2),((1,1),3)]
toArray :: [[a]] -> Array (Int, Int) a
toArray l = array bound associations
    where r = length l
          c = length $ head l
          bound = ((0, 0), (r - 1, c - 1))
          cIndices = map (zip [0..c-1]) l
          rcIndices = zip [0..r-1] cIndices
          associations = concatMap associate rcIndices
          associate (r', cs) = map (\(c', i) -> ((r', c'), i)) cs

data Cost a = C a | Inf deriving (Show, Eq)
instance (Ord a) => Ord (Cost a) where
    Inf <= Inf = True
    Inf <= C _ = False
    C _ <= Inf = True
    C c <= C d = c <= d
addCost :: Num a => Cost a -> Cost a -> Cost a
addCost (C c) (C d) = C (c + d)
addCost _ _ = Inf

data Q a ix = Q (Cost a) ix deriving (Show, Eq, Ord)

dijkstrasArr' :: Ix ix
    => Array ix a
    -- ^ starting array
    -> M.Map ix (Cost Int)
    -- ^ distances to each position in the array
    -> M.Map ix ix
    -- ^ maps each position in the array to its predecessor in the shortest path
    -> S.Set (Q Int ix)
    -- ^ queue for exploration
    -> (Array ix a -> ix -> Cost Int)
    -- ^ cost function. assumes cost is a function of only the next position, not the previous
    -> (Array ix a -> ix -> [ix])
    -- ^ get neighbors of a position in the array
    -> M.Map ix (Cost Int)
    -- ^ output of the costs to reach each position in the array
dijkstrasArr' orig dist predecessor q cost neighbors
    | S.null q = dist
    | otherwise = dijkstrasArr' orig dist' pred' q'' cost neighbors
        where (Q _ u, q') = S.deleteFindMin q
              uns = neighbors orig u
              (dist', pred', q'') = foldr funs (dist, predecessor, q') uns
              funs v (d', p', queue) = (d'', p'', queue')
                  where alt = addCost (M.findWithDefault Inf u d') (cost orig v)
                        distV = M.findWithDefault Inf v d'
                        (d'', p'', queue') | alt < distV = (M.insert v alt d', M.insert v u p', S.insert (Q alt v) queue)
                                           | otherwise = (d', p', queue)

dijkstrasArr :: Ix ix
    => Array ix a
    -- ^ starting array
    -> [ix]
    -- ^ starting position
    -> (Array ix a -> ix -> Cost Int)
    -- ^ cost function. assumes cost is a function of only the next position, not the previous
    -> (Array ix a -> ix -> [ix])
    -- ^ get neighbors of a position in the array
    -> M.Map ix (Cost Int)
dijkstrasArr orig sources = dijkstrasArr' orig dist predecessor queue
    where b = bounds orig
          sourcesSet = S.fromList sources
          (dist, queue) = foldr initialize (M.empty, S.empty) $ range b
          initialize xy (d, q) =
            let cost = if S.member xy sourcesSet then C 0 else Inf in
            (M.insert xy cost d, S.insert (Q cost xy) q)
          predecessor = M.empty

-- -- A specific case of dijkstras when it's possible to run it on an array.
-- -- This works, but runs slowly because it needs to copy all the arrays to each state.
-- -- Treat state as (Array ix a, ix)
-- dijkstrasArr :: (Ix ix, Eq a, Ord a)
--     => Array ix a
--     -- ^ starting array
--     -> [ix]
--     -- ^ starting position
--     -> (Array ix a -> ix -> Cost Int)
--     -- ^ cost function. assumes cost is a function of only the next position, not the previous
--     -> (Array ix a -> ix -> [ix])
--     -- ^ get neighbors of a position in the array
--     -> M.Map ix (Cost Int)
--     -- ^ output of the costs to reach each position in the array
-- dijkstrasArr orig sources cost neighbors = M.mapKeys snd finalStates
--     where arrStates = zip (repeat orig) sources
--           cost' _ (_, nextIx) = cost orig nextIx
--           neighbors' (a, ix) = zip (repeat a) (neighbors a ix)
--           finalStates = dijkstras arrStates cost' neighbors'

astar' :: Ord state
    => M.Map state (Cost Int)
    -- ^ distances to each state
    -> M.Map state state
    -- ^ maps each state to its predecessor in the shortest path
    -> S.Set (Q Int state)
    -- ^ queue for exploration
    -> (state -> Cost Int)
    -- ^ heuristic function
    -> (state -> state -> Cost Int)
    -- ^ cost function between prev and next state
    -> (state -> [state])
    -- ^ get neighbors of a state
    -> M.Map state (Cost Int)
    -- ^ output of the costs to reach each state
astar' dist predecessor q heuristic cost neighbors
    | S.null q = dist
    | otherwise = astar' dist' pred' q'' heuristic cost neighbors
        where (Q _ u, q') = S.deleteFindMin q
              uns = neighbors u
              (dist', pred', q'') = foldr funs (dist, predecessor, q') uns
              funs v (d', p', queue) = (d'', p'', queue')
                  where alt = addCost (heuristic v) $ addCost (M.findWithDefault Inf u d') (cost u v)
                        distV = M.findWithDefault Inf v d'
                        (d'', p'', queue') | alt < distV = (M.insert v alt d', M.insert v u p', S.insert (Q alt v) queue)
                                           | otherwise = (d', p', queue)

astar :: Ord state
    => [state]
    -- ^ starting states
    -> (state -> Cost Int)
    -- ^ heuristic function
    -> (state -> state -> Cost Int)
    -- ^ cost function. assumes cost is a function of only the next position, not the previous
    -> (state -> [state])
    -- ^ get neighbors of a position in the array
    -> M.Map state (Cost Int)
    -- ^ output of the costs to reach each state
astar sources = astar' dist predecessor queue
    where sourcesSet = S.fromList sources
          (dist, queue) = S.foldr initialize (M.empty, S.empty) sourcesSet
          initialize s (d, q) =
            let cost = if S.member s sourcesSet then C 0 else Inf in
            (M.insert s cost d, S.insert (Q cost s) q)
          predecessor = M.empty

dijkstras :: Ord state
    => [state]
    -- ^ starting states
    -> (state -> state -> Cost Int)
    -- ^ cost function. assumes cost is a function of only the next position, not the previous
    -> (state -> [state])
    -- ^ get neighbors of a position in the array
    -> M.Map state (Cost Int)
    -- ^ output of the costs to reach each state
dijkstras sources = astar sources (const $ C 0)
