module Day01.Solution (solve) where

import Data.List
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day01/input.txt"
    putStrLn $ "2022.01.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.01.2: " ++ show (solve2 $ parseInput input)

parseElf :: CharParser st [Int]
parseElf = do
    nums <- endBy1 (many1 digit) newline
    return $ map read nums

parseElves :: CharParser st [[Int]]
parseElves = sepBy1 parseElf newline

parseInput :: String -> [[Int]]
parseInput i = case parse parseElves "" i of
    Left e -> error (show e)
    Right es -> es

solve1 :: [[Int]] -> Int
solve1 elves = maximum $ map sum elves

solve2 :: [[Int]] -> Int
solve2 elves = (sum . take 3 . reverse . sort) $ map sum elves
