module Day09.Solution (solve) where

import Data.List.Split
import Data.List ( foldl' )

solve :: IO ()
solve = do
    input <- readFile "./src/Day09/input.txt"
    putStrLn $ "2023.09.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.09.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> [[Int]]
parseInput i = map (map read . splitOn " ") (lines i)

allZeros :: [Int] -> Bool
allZeros = all (== 0)

extrapolateNext :: [[Int]] -> Int
extrapolateNext = foldl' (\acc s -> acc + last s) 0

extrapolatePrev :: [[Int]] -> Int
extrapolatePrev = foldl' (\acc s -> head s - acc) 0

seqDiff :: [Int] -> [Int]
seqDiff s = snd $ foldr (\currVal (nextVal, acc) -> (currVal, nextVal - currVal : acc)) (last s, []) (init s)

allDiffs :: [Int] -> [[Int]] -> [[Int]]
allDiffs s acc =
    if allZeros s then acc
    else allDiffs (seqDiff s) (s:acc)

solve1 :: [[Int]] -> Int
solve1 = sum . map (extrapolateNext . (`allDiffs` []))

solve2 :: [[Int]] -> Int
solve2 = sum . map (extrapolatePrev . (`allDiffs` []))
