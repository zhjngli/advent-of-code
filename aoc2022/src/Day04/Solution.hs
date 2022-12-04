module Day04.Solution (solve) where

import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day04/input.txt"
    putStrLn $ "2022.04.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.04.2: " ++ show (solve2 $ parseInput input)

type Range = (Int, Int)
type ElfPair = (Range, Range)

parseRange :: CharParser st Range
parseRange = do
    x1 <- many1 digit
    _ <- char '-'
    x2 <- many1 digit
    return (read x1, read x2)

parseElfPair :: CharParser st ElfPair
parseElfPair = do
    r1 <- parseRange
    _ <- char ','
    r2 <- parseRange
    return (r1, r2)

parseElfPairs :: CharParser st [ElfPair]
parseElfPairs = endBy1 parseElfPair newline

parseInput :: String -> [ElfPair]
parseInput i = case parse parseElfPairs "" i of
    Left e -> error (show e)
    Right es -> es

rangeFullyOverlaps :: Range -> Range -> Bool
rangeFullyOverlaps (x1, x2) (y1, y2) = y1Between && y2Between
    where y1Between = x1 <= y1 && x2 >= y1
          y2Between = x1 <= y2 && x2 >= y2

rangeOverlaps :: Range -> Range -> Bool
rangeOverlaps (x1, x2) (y1, y2) = y1Between || y2Between
    where y1Between = x1 <= y1 && x2 >= y1
          y2Between = x1 <= y2 && x2 >= y2

solve' :: (Range -> Range -> Bool) -> [ElfPair] -> Int
solve' r es = length $ filter p es
    where p (r1, r2) = r r1 r2 || r r2 r1

solve1 :: [ElfPair] -> Int
solve1 = solve' rangeFullyOverlaps

solve2 :: [ElfPair] -> Int
solve2 = solve' rangeOverlaps
