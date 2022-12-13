module Lib.Common (
    toArray,
    Cost(..),
    Q,
    addCost,
    dijkstras2D
) where

import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S

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

data Q a = Q (Cost a) (Int, Int) deriving (Show, Eq, Ord)

dijkstras2D' :: Array (Int, Int) a
    -- ^ starting array
    -> M.Map (Int, Int) (Cost Int)
    -- ^ distances to each position in the array
    -> M.Map (Int, Int) (Int, Int)
    -- ^ maps each position in the array to its predecessor in the shortest path
    -> S.Set (Q Int)
    -- ^ queue for exploration
    -> (Array (Int, Int) a -> (Int, Int) -> Cost Int)
    -- ^ cost function. assumes cost is a function of only the next position, not the previous
    -> (Array (Int, Int) a -> (Int, Int) -> [(Int, Int)])
    -- ^ get neighbors of a position in the array
    -> M.Map (Int, Int) (Cost Int)
    -- ^ output of the costs to reach each position in the array
dijkstras2D' orig dist predecessor q cost neighbors
    | S.null q = dist
    | otherwise = dijkstras2D' orig dist' pred' q'' cost neighbors
        where (Q _ (ux, uy), q') = S.deleteFindMin q
              uns = neighbors orig (ux, uy)
              (dist', pred', q'') = foldr funs (dist, predecessor, q') uns
              funs (vx, vy) (d', p', queue) = (d'', p'', queue')
                  where alt = addCost (M.findWithDefault Inf (ux, uy) d') (cost orig (vx, vy))
                        distV = M.findWithDefault Inf (vx, vy) d'
                        (d'', p'', queue') | alt < distV = (M.insert (vx, vy) alt d', M.insert (vx, vy) (ux, uy) p', S.insert (Q alt (vx, vy)) queue)
                                           | otherwise = (d', p', queue)

-- could probably factor out (Int, Int) as a type variable
dijkstras2D :: Array (Int, Int) a
    -- ^ starting array
    -> [(Int, Int)]
    -- ^ starting position
    -> (Array (Int, Int) a -> (Int, Int) -> Cost Int)
    -- ^ cost function. assumes cost is a function of only the next position, not the previous
    -> (Array (Int, Int) a -> (Int, Int) -> [(Int, Int)])
    -- ^ get neighbors of a position in the array
    -> M.Map (Int, Int) (Cost Int)
dijkstras2D orig sources = dijkstras2D' orig dist predecessor queue
    where b = bounds orig
          sourcesSet = S.fromList sources
          (dist, queue) = foldr initialize (M.empty, S.empty) $ range b
          initialize xy (d, q) =
            let cost = if S.member xy sourcesSet then C 0 else Inf in
            (M.insert xy cost d, S.insert (Q cost xy) q)
          predecessor = M.empty
