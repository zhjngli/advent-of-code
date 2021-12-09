module Y2021.Day08.Solution where

import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day08/input.txt"
    putStrLn $ "2021.08.1: " ++ show (solve1 $ parseInput input)

parseLine :: CharParser st ([String], [String])
parseLine = do
    input <- endBy1 (many1 letter) (char ' ')
    string "| "
    output <- sepBy1 (many1 letter) (char ' ')
    return (input, output)

parseLines :: CharParser st [([String], [String])]
parseLines = endBy parseLine newline

parseInput :: String -> [([String], [String])]
parseInput i = case parse parseLines "" i of
    Left e -> []
    Right lines -> lines

solve1 :: [([String], [String])] -> Int
solve1 l = sum $ map count1478 l
    where count1478 (_, output) = foldr f 0 output
          f out c
            | length out == 2 = c + 1
            | length out == 3 = c + 1
            | length out == 4 = c + 1
            | length out == 7 = c + 1
            | otherwise = c
