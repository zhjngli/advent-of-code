module Y2021.Day01.Solution where

import Data.List

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day01/input.txt"
    let testInput1 = read <$> lines input
    print $ solve1 testInput1


solve1 :: [Int] -> Int
solve1 depths = fst $ foldl' (\(count, previousDepth) depth -> if depth > previousDepth then (count + 1, depth) else (count, depth)) (0, head depths) depths
