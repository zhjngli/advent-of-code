{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Day06.Solution (solve) where

import Data.List.Split

solve :: IO ()
solve = do
    input <- readFile "./src/Day06/input.txt"
    putStrLn $ "2023.06.1: " ++ show (solve' $ parseInput1 input)
    putStrLn $ "2023.06.2: " ++ show (solve' $ parseInput2 input)

parseInput1 :: String -> [(Int, Int)]
parseInput1 i = zip ts ds
    where
        ts = parse $ splitOn " " time
        ds = parse $ splitOn " " distance
        parse (_:ns) = map read $ filter (/= "") ns
        parse [] = error "empty time or distance list"
        [time, distance] =
            case lines i of
            [t, d] -> [t, d]
            _ -> error "improper format"

parseInput2 :: String -> [(Int, Int)]
parseInput2 i = [(ts, ds)]
    where
        ts = parse $ splitOn " " time
        ds = parse $ splitOn " " distance
        parse (_:ns) = (read . concat . filter (/= "")) ns
        parse [] = error "empty time or distance list"
        [time, distance] =
            case lines i of
            [t, d] -> [t, d]
            _ -> error "improper format"

solve' :: [(Int, Int)] -> Int
solve' = product . map calcWays
    where calcWays (t, d) = (length . filter (\s -> s * (t-s) > d)) [1..t]
