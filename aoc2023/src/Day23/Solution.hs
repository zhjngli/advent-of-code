{-# LANGUAGE TupleSections #-}
module Day23.Solution (solve) where

import Data.Array
import Lib.Common
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Sequence as Q
import Data.Foldable

solve :: IO ()
solve = do
    input <- readFile "./src/Day23/input.txt"
    putStrLn $ "2023.23.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.23.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> Array (Int, Int) Char
parseInput = toArray . lines

neighbors :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
neighbors a (r, c) = filter (\ix -> inRange b ix && a ! ix /= '#') [(r, c-1), (r, c+1), (r-1, c), (r+1, c)]
    where b = bounds a

-- current ix, visited ixs
type PathState = ((Int, Int), S.Set (Int, Int))

canMoveTo :: Array (Int, Int) Char -> PathState -> [PathState]
canMoveTo a (ix@(r, c), visited) = case a ! ix of
    '.' -> states
    '>' -> filter (\((r', c'), _) -> r' == r && c' == c+1) states
    '^' -> filter (\((r', c'), _) -> r' == r-1 && c' == c) states
    '<' -> filter (\((r', c'), _) -> r' == r && c' == c-1) states
    'v' -> filter (\((r', c'), _) -> r' == r+1 && c' == c) states
    '#' -> error "you shouldn't be on a forest tile"
    ch -> error ("unknown character: " ++ [ch])
    where
        ns = filter (`S.notMember` visited) (neighbors a ix)
        states = map (\n -> (n, S.insert n visited)) ns

-- it's hella bad practice to use a negative cost weight for dijkstras, but
-- 1. all weights are negative (i.e. you'll never visit a node with positive cost) so it's fine as long as there are no cycles
-- 2. there are no cycles cause of the no revisiting a tile constraint, which is encoded in the state
cost :: PathState -> PathState -> Cost Int
cost _ _ = C (-1)

solve1 :: Array (Int, Int) Char -> Cost Int
solve1 a = minimum $ M.filterWithKey (\(i, _) _ -> i == end) costs
    where
        (_, (maxr, maxc)) = bounds a
        end = (maxr, maxc-1)
        (costs, _) = dijkstras [((0, 1), S.empty)] cost (canMoveTo a)

-- canMoveTo2 :: Array (Int, Int) Char -> PathState -> [PathState]
-- canMoveTo2 a (ix, visited) = case a ! ix of
--     '.' -> states
--     '>' -> states
--     '^' -> states
--     '<' -> states
--     'v' -> states
--     '#' -> error "you shouldn't be on a forest tile"
--     ch -> error ("unknown character: " ++ [ch])
--     where
--         ns = filter (`S.notMember` visited) (neighbors a ix)
--         states = map (\n -> (n, S.insert n visited)) ns

-- solve2BruteForce :: Array (Int, Int) Char -> Cost Int
-- solve2BruteForce a = minimum $ M.filterWithKey (\(i, _) _ -> i == end) costs
--     where
--         (_, (maxr, maxc)) = bounds a
--         end = (maxr, maxc-1)
--         (costs, _) = dijkstras [((0, 1), S.empty)] cost (canMoveTo2 a)

isCrossing :: Array (Int, Int) Char -> (Int, Int) -> Bool
isCrossing a i = a ! i /= '#' && length (neighbors a i) /= 2

crossings :: Array (Int, Int) Char -> [(Int, Int)]
crossings a = foldl' (\acc i -> if isCrossing a i then i:acc else acc) [] (indices a)

nearestCrossings :: Array (Int, Int) Char -> (Int, Int) -> [((Int, Int), Int)]
nearestCrossings a i = nearestCrossings' S.empty [(i, 0)] []
    where
        nearestCrossings' _ [] nearest = nearest
        nearestCrossings' seen ((p, n):toProcess) nearest = nearestCrossings' seen' toProcess' nearest'
            where
                seen' = S.insert p seen
                ns = neighbors a p
                crosses = filter (\ix -> S.notMember ix seen' && isCrossing a ix) ns
                nearest' = map (, n+1) crosses ++ nearest
                moreToProcess = filter (\ix -> S.notMember ix seen' && not (isCrossing a ix)) ns
                toProcess' = map (, n+1) moreToProcess ++ toProcess

reducedGraph :: Array (Int, Int) Char -> M.Map (Int, Int) (M.Map (Int, Int) Int)
reducedGraph a = reducedGraph' M.empty (crossings a)
    where
        reducedGraph' m [] = m
        reducedGraph' m (c:cs) = reducedGraph' m' cs
            where
                ncs = nearestCrossings a c
                m' = foldl' (\g (i, n) -> M.insertWith M.union c (M.singleton i n) g) m ncs

-- FYI using a sequence makes a huge difference cause i don't have to deal with list concatenation
longest :: M.Map (Int, Int) (M.Map (Int, Int) Int) -> (Int, Int) -> (Int, Int) -> Int
longest m s e = longest' 0 (Q.singleton (s, 0, S.singleton s))
    where
        longest' currLongest q =
            if Q.null q then currLongest
            else
                let (p, l, seen) = Q.index q 0
                    qs = Q.drop 1 q
                    ns = M.assocs (m M.! p)
                    newQs = foldl' (\acc (n, dist) -> if S.member n seen then acc else acc Q.|> (n, l + dist, S.insert n seen)) qs ns
                in
                if p == e then longest' (max currLongest l) qs
                else longest' currLongest newQs

solve2 :: Array (Int, Int) Char -> Int
solve2 a = longest m start end
    where
        ((minr, minc), (maxr, maxc)) = bounds a
        end = (maxr, maxc-1)
        start = (minr, minc+1)
        m = reducedGraph a
