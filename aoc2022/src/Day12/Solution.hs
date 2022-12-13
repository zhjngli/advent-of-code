module Day12.Solution (solve) where

import Lib.Common

-- import Debug.Trace
import Data.Array
import qualified Data.Map as M
import Data.List ( elemIndex )
import Data.Maybe

solve :: IO ()
solve = do
    input <- readFile "./src/Day12/input.txt"
    let heightMap = lines input
    putStrLn $ "2022.12.1: " ++ show (solve1 heightMap)
    putStrLn $ "2022.12.2: " ++ show (solve2 heightMap)

heightMapNeighbors :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
heightMapNeighbors a p@(r, c) = filter f [(r+x, c+y) | (x, y) <- [(1, 0), (0, 1), (-1, 0), (0, -1)]]
    where b = bounds a
          f p' = inRange b p' && canStep (a ! p) (a ! p')

heights :: [Char]
heights = "abcdefghijklmnopqrstuvwxyz"

canStep :: Char -> Char -> Bool
canStep 'S' 'E' = False
canStep 'S' to  = fromJust (elemIndex to heights) <= 1
canStep 'E' _   = True
canStep _   'S' = True
canStep from 'E' = 25 - fromJust (elemIndex from heights) <= 1
canStep from to  = fromJust (elemIndex to heights) - fromJust (elemIndex from heights) <= 1

heightMapCost :: Array (Int, Int) Char -> (Int, Int) -> Cost Int
heightMapCost _ _ = C 1

solve1 :: [[Char]] -> Cost Int
solve1 cs = M.findWithDefault Inf end $ dijkstrasArr heightMap [source] heightMapCost heightMapNeighbors
    where heightMap = toArray cs
          (source, end) = foldr f ((0, 0), (0, 0)) $ assocs heightMap
          f (x, 'S') (_, e) = (x, e)
          f (x, 'E') (s, _) = (s, x)
          f (_, _)   se     = se

solve2 :: [[Char]] -> Cost Int
solve2 cs = M.findWithDefault Inf end $ dijkstrasArr heightMap sources heightMapCost heightMapNeighbors
    where heightMap = toArray cs
          (sources, end) = foldr f ([], (0, 0)) $ assocs heightMap
          f (x, 'S') (ss, e) = (x:ss, e)
          f (x, 'a') (ss, e) = (x:ss, e)
          f (x, 'E') (ss, _) = (ss, x)
          f (_, _)   se     = se
