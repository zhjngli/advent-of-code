module Day20.Solution (solve) where

-- import Debug.Trace
import Data.List ( foldl', elemIndex )
import Data.Maybe

solve :: IO ()
solve = do
    input <- readFile "./src/Day20/input.txt"
    putStrLn $ "2022.20.1: " ++ show (solve1 $ map read (lines input))
    putStrLn $ "2022.20.2: " ++ show (solve2 $ map read (lines input))

moveRight :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
moveRight left ic right c =
    let lr = length right in
    if c > lr then moveLeft left ic right (length left - c + lr)
    else left ++ take c right ++ [ic] ++ drop c right

moveLeft :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)] -> Int -> [(Int, Int)]
moveLeft left ic right c =
    let ll = length left in
    if c > ll then moveRight left ic right (length right - c + ll)
    else let llx = ll - c in take llx left ++ [ic] ++ drop llx left ++ right

mix :: [(Int, Int)] -> Int -> [(Int, Int)]
mix indexedCoords n
  | c == 0 = indexedCoords
  | c > 0 = moveRight left (i, c') right c
  | c < 0 = moveLeft left (i, c') right (abs c)
  | otherwise = error "all cases covered"
  where
      (left, rest) = break (\(j, _) -> j == n) indexedCoords
      (i, c') = head rest
      right = tail rest
      c = c' `mod` (length indexedCoords - 1)

mixAll :: Int -> [(Int, Int)] -> [(Int, Int)]
mixAll 0 ics = ics
mixAll n ics = mixAll (n-1) (foldl' mix ics [0..length ics -1])

solve' :: Int -> [Int] -> Int
solve' n is = mixedIs !! one + mixedIs !! two + mixedIs !! three
    where l = length is
          indexedIs = zip [0..l-1] is
          mixedIs = map snd $ mixAll n indexedIs
          zeroI = fromJust $ elemIndex 0 mixedIs
          one = (zeroI + 1000) `mod` l
          two = (zeroI + 2000) `mod` l
          three = (zeroI + 3000) `mod` l

solve1 :: [Int] -> Int
solve1 = solve' 1

solve2 :: [Int] -> Int
solve2 is = solve' 10 $ map (811589153 *) is
