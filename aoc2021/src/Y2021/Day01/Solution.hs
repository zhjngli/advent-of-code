module Y2021.Day01.Solution where

import Data.List

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day01/input.txt"
    let depths = read <$> lines input
    putStrLn $ "2021.01.1: " ++ show (solve1 depths)
    putStrLn $ "2021.01.2: " ++ show (solve2 depths)


solve1 :: [Int] -> Int
solve1 depths = fst $ foldl' (\(count, previousDepth) depth -> if depth > previousDepth then (count + 1, depth) else (count, depth)) (0, head depths) depths

solve2 :: [Int] -> Int
solve2 depths = count
    where (count, _, _, _) = foldl' (\(c, d1, d2, d3) d -> if d > d3 then (c + 1, d, d1, d2) else (c, d, d1, d2))
                                    (0, head $ tail $ tail depths, head $ tail depths, head depths)
                                    (tail $ tail $ tail depths)
