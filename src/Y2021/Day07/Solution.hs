module Y2021.Day07.Solution where

import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day07/input.txt"
    putStrLn $ "2021.07.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.07.2: " ++ show (solve2 $ parseInput input)

parseCrabs :: CharParser st [Int]
parseCrabs = do
    crabs <- sepBy (many1 digit) (char ',')
    newline
    return $ map read crabs

parseInput :: String -> [Int]
parseInput i = case parse parseCrabs "" i of
    Left e -> []
    Right c -> c

totalFuel :: [Int] -> (Int -> Int) -> Int -> Int
totalFuel i f m = foldr (\i a -> a + f (abs (i - m))) 0 i

triangleNum :: Int -> Int
triangleNum 0 = 0
triangleNum x = x + triangleNum (x - 1)

possibleTotals :: [Int] -> [Int] -> (Int -> Int) -> [Int]
possibleTotals range i f = map (totalFuel i f) range

solve1 :: [Int] -> Int
solve1 i = minimum $ possibleTotals [minimum i .. maximum i] i id

solve2 :: [Int] -> Int
solve2 i = minimum $ possibleTotals [minimum i .. maximum i] i triangleNum
