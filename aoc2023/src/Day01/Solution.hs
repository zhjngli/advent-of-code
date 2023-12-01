module Day01.Solution (solve) where

import Text.ParserCombinators.Parsec
import Data.Text (replace, pack, unpack)

solve :: IO ()
solve = do
    input <- readFile "./src/Day01/input.txt"
    -- test <- readFile "./src/Day01/test.txt"
    putStrLn $ "2023.01.1: " ++ show (solve' $ parseInput (lines input))
    -- print (parseInput2 (lines test))
    putStrLn $ "2023.01.2: " ++ show (solve' $ parseInput2 (lines input))

parseDigit :: CharParser st Int
parseDigit = do
    skipMany letter
    d <- digit
    return $ read [d]

parseValue :: String -> Int
parseValue i = case parse parseDigit "" i of
    Left e -> error (show e)
    Right v -> v

parseInput :: [String] -> [(Int, Int)]
parseInput = map (\v -> (parseValue v, parseValue $ reverse v))

parseInput2 :: [String] -> [(Int, Int)]
parseInput2 vs = parseInput $ map digitReplace vs
    where digitReplace s = let t = pack s in
            (unpack .
            replace (pack "one")   (pack "o1e") .
            replace (pack "two")   (pack "t2o") .
            replace (pack "three") (pack "th3ee") .
            replace (pack "four")  (pack "f4ur") .
            replace (pack "five")  (pack "f5ve") .
            replace (pack "six")   (pack "s6x") .
            replace (pack "seven") (pack "se7en") .
            replace (pack "eight") (pack "ei8ht") .
            replace (pack "nine")  (pack "n9ne") .
            replace (pack "zero")  (pack "z0ro")) t

solve' :: [(Int, Int)] -> Int
solve' = sum . map (\(d1, d2) -> 10 * d1 + d2)
