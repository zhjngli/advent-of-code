module Day06.Solution (solve) where

-- import Debug.Trace
import Data.List ( foldl' )

solve :: IO ()
solve = do
    input <- readFile "./src/Day06/input.txt"
    putStrLn $ "2022.06.1: " ++ show (solve1 input)
    putStrLn $ "2022.06.2: " ++ show (solve2 input)

allDifferent :: [Char] -> Bool
allDifferent [] = True
allDifferent (c:cs) = allDifferent cs && foldr f True cs
    where f c' b = b && c /= c'

solve' :: Int -> String -> Int
solve' n s = snd $ foldl' f (first, n) s'
    where s' = drop n s
          first = take n s
          f (letters, n') c = if allDifferent letters then (letters, n') else (tail letters ++ [c], n' + 1) 

solve1 :: String -> Int
solve1 = solve' 4

solve2 :: String -> Int
solve2 = solve' 14
