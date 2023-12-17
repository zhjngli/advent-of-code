module Day17.Solution (solve) where

import Lib.Common (dijkstras, toArray, Cost (..))
import Data.Array
import Data.Char (digitToInt)
import qualified Data.Map as M

solve :: IO ()
solve = do
    input <- readFile "./src/Day17/input.txt"
    putStrLn $ "2023.17.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.17.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> Array (Int, Int) Int
parseInput = toArray . map (map digitToInt) . lines

data Dir = U | D | L | R deriving (Show, Eq, Ord)
-- ix in the grid, direction it's heading, number of times it's already moved in that direction
type CrucibleState = ((Int, Int), Dir, Int)

turns :: CrucibleState -> [CrucibleState]
turns ((r, c), d, n) = case d of -- 3 elems each. turn left, turn right, or go straight
    U -> [((r, c-1), L, 1), ((r, c+1), R, 1), ((r-1, c), U, n+1)]
    D -> [((r, c+1), R, 1), ((r, c-1), L, 1), ((r+1, c), D, n+1)]
    L -> [((r+1, c), D, 1), ((r-1, c), U, 1), ((r, c-1), L, n+1)]
    R -> [((r-1, c), U, 1), ((r+1, c), D, 1), ((r, c+1), R, n+1)]

canMoveTo :: Array (Int, Int) a -> CrucibleState -> [CrucibleState]
canMoveTo a s = filter (\(i, _, n') -> inRange (bounds a) i && n' <= 3) (turns s)

heatLoss :: Array (Int, Int) Int -> CrucibleState -> CrucibleState -> Cost Int
heatLoss a _ (i, _, _) = C $ a ! i

solve1 :: Array (Int, Int) Int -> Cost Int
solve1 a = minimum $ M.filterWithKey (\(i, _, _) _ -> i == end) finalHeatLoss
    where
        (_, end) = bounds a
        (finalHeatLoss, _) = dijkstras [((0, 0), R, 0), ((0, 0), D, 0)] (heatLoss a) (canMoveTo a)

canMoveTo2 :: Array (Int, Int) a -> CrucibleState -> [CrucibleState]
canMoveTo2 a s@(_, d, n) = filter (\(i, d', n') -> inRange (bounds a) i && n' <= 10 && (n >= 4 || d' == d)) (turns s)

solve2 :: Array (Int, Int) Int -> Cost Int
solve2 a = minimum $ M.filterWithKey (\(i, _, n) _ -> i == end && n >= 4) finalHeatLoss
    where
        (_, end) = bounds a
        (finalHeatLoss, _) = dijkstras [((0, 0), R, 0), ((0, 0), D, 0)] (heatLoss a) (canMoveTo2 a)
