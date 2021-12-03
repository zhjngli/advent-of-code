module Y2021.Day02.Solution where

import Data.List

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day02/input.txt"
    let movements = map (\[d, amount] -> (d, read amount)) (words <$> lines input)
    putStrLn $ "2021.02.1: " ++ show (solve1 movements)
    putStrLn $ "2021.02.2: " ++ show (solve2 movements)


solve1 :: [(String, Int)] -> Int
solve1 movements = forward * depth
    where (forward, depth) = foldl' pos (0, 0) movements
          pos (f, d) ("forward", x) = (f + x, d)
          pos (f, d) ("down",    x) = (f, d + x)
          pos (f, d) ("up",      x) = (f, d - x)
          pos _ _ = error "Invalid direction"

solve2 :: [(String, Int)] -> Int
solve2 movements = forward * depth
    where (forward, aim, depth) = foldl' pos (0, 0, 0) movements
          pos (f, a, d) ("forward", x) = (f + x, a, d + x * a)
          pos (f, a, d) ("down",    x) = (f, a + x, d)
          pos (f, a, d) ("up",      x) = (f, a - x, d)
          pos _ _ = error "Invalid direction"
