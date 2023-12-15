module Day14.Solution (solve) where

import Data.List ( sort, intercalate, transpose )
import Data.List.Split
import qualified Data.Map as M

solve :: IO ()
solve = do
    input <- readFile "./src/Day14/input.txt"
    putStrLn $ "2023.14.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.14.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> [String]
parseInput = lines

east :: [String] -> [String]
east = map (intercalate "#" . map sort . splitOn "#")

west :: [String] -> [String]
west = map reverse . east . map reverse

north :: [String] -> [String]
north = transpose . west . transpose

south :: [String] -> [String]
south = transpose . east . transpose

spin :: [String] -> [String]
spin = east . south . west . north

totalLoad :: [String] -> Int
totalLoad = sum . zipWith (\ i r -> i * length (filter (== 'O') r)) [1..] . reverse

solve1 :: [String] -> Int
solve1 = totalLoad . north

detectCycle :: [String] -> Int -> M.Map [String] Int -> (Int, Int)
detectCycle a i known =
    case M.lookup a known of
    Just s -> (s, i - s)
    Nothing -> detectCycle (spin a) (i + 1) (M.insert a i known)

solve2 :: [String] -> Int
solve2 a = totalLoad a'
    where
        (cycleStart, cycleLength) = detectCycle a 0 M.empty
        totalSpins = 1000000000
        extraSpins = cycleStart + (totalSpins - cycleStart) `mod` cycleLength
        a' = iterate spin a !! extraSpins
