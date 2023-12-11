{-# LANGUAGE TupleSections #-}
module Day11.Solution (solve) where

import Data.Array
import Lib.Common (toArray)

solve :: IO ()
solve = do
    input <- readFile "./src/Day11/input.txt"
    putStrLn $ "2023.11.1: " ++ show (solve' 2 $ parseInput input)
    putStrLn $ "2023.11.2: " ++ show (solve' 1000000 $ parseInput input)

type Space = Array (Int, Int) Char

expand :: Space -> ([Int], [Int])
expand space = (rowsToExpand, colsToExpand)
    where
        ((r0, c0), (r, c)) = bounds space
        rowsToExpand = foldr (\r' acc -> if all ((== '.') . (space !) . (r',)) [c0..c] then r':acc else acc) [] [r0..r]
        colsToExpand = foldr (\c' acc -> if all ((== '.') . (space !) . (,c')) [r0..r] then c':acc else acc) [] [c0..c]

galaxies :: Space -> [(Int, Int)]
galaxies = map fst . filter ((== '#') . snd) . assocs

parseInput :: String -> Space
parseInput input = toArray (lines input)

solve' :: Int -> Space -> Int
solve' i p = sum [dist g1 g2 | g1 <- gs, g2 <- gs, g1 /= g2] `div` 2 -- sum is doubled cause list comprehension gets all permutations not combinations
    where
        gs = galaxies p
        (rs, cs) = expand p
        within lower upper = length . filter (\x -> x >= lower && x <= upper)
        extraSpace = i - 1
        dist (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2) + extraSpace * extraRows + extraSpace * extraCols
            where extraRows = within (min r1 r2) (max r1 r2) rs
                  extraCols = within (min c1 c2) (max c1 c2) cs
