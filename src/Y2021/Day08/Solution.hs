module Y2021.Day08.Solution where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day08/input.txt"
    putStrLn $ "2021.08.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.08.2: " ++ show (solve2 $ parseInput input)

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

solve2 :: [([String], [String])] -> Int
solve2 = foldr (\l a -> decodeDigits l + a) 0

decodeDigits :: ([String], [String]) -> Int
decodeDigits (input, output) = read $ concatMap outputValue outputSets
    where inputSets = map S.fromList input
          outputSets = map S.fromList output
          segmentCounts = M.toList $ foldr count M.empty inputSets
              where count s m = foldr (\c m -> if M.member c m then M.adjust (+1) c m else M.insert c 1 m) m s
          segmentE = head [s | (s, c) <- segmentCounts, c == 4]
          segmentB = head [s | (s, c) <- segmentCounts, c == 6]
          segmentF = head [s | (s, c) <- segmentCounts, c == 9]
          segmentDG     = [s | (s, c) <- segmentCounts, c == 7]
          one   = head $ [s | s <- inputSets, S.size s == 2]
          seven = head $ [s | s <- inputSets, S.size s == 3]
          four  = head $ [s | s <- inputSets, S.size s == 4]
          eight = head $ [s | s <- inputSets, S.size s == 7]
          zero  = head $ [s | s <- inputSets, S.size s == 6, S.member segmentB s, S.member segmentE s, not $ all (`S.member` s) segmentDG]
          nine  = head $ [s | s <- inputSets, S.size s == 6, S.member segmentB s, not (S.member segmentE s)]
          six   = head $ [s | s <- inputSets, S.size s == 6, S.member segmentB s, S.member segmentE s, all (`S.member` s) segmentDG]
          two   = head $ [s | s <- inputSets, S.size s == 5, not (S.member segmentB s), S.member segmentE s, not (S.member segmentF s)]
          three = head $ [s | s <- inputSets, S.size s == 5, not (S.member segmentB s), not (S.member segmentE s), S.member segmentF s]
          five  = head $ [s | s <- inputSets, S.size s == 5, S.member segmentB s, not (S.member segmentE s), S.member segmentF s]
          outputValue o = case elemIndex o [zero, one, two, three, four, five, six, seven, eight, nine] of
              Just v -> show v
              Nothing -> error "can't find this output value"
