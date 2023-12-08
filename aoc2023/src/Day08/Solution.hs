module Day08.Solution (solve) where

import Data.Map (Map, fromList, (!), keys)
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day08/input.txt"
    putStrLn $ "2023.08.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.08.2: " ++ show (solve2 $ parseInput input)

type Network = Map String (String, String)

parseNetwork :: CharParser st (String, Network)
parseNetwork = do
    instrs <- many1 (char 'L' <|> char 'R')
    _ <- newline
    _ <- newline
    nodes <- endBy1 parseNode newline
    return (instrs, fromList nodes)
    where parseNode = do
            name <- many1 upper
            _ <- string " = ("
            left <- many1 upper
            _ <- string ", "
            right <- many1 upper
            _ <- string ")"
            return (name, (left, right))

parseInput :: String -> (String, Network)
parseInput i = case parse parseNetwork "" i of
    Left e -> error (show e)
    Right v -> v

countSteps :: Network -> String -> (String -> Bool) -> Int -> String -> Int
countSteps network instrs endCondition steps curr =
    if endCondition curr then steps
    else countSteps network (tail instrs) endCondition (steps+1) (step $ network ! curr) 
    where
        step = case head instrs of
            'L' -> fst
            'R' -> snd
            i -> error ("unrecognized instruction: " ++ [i])

solve1 :: (String, Network) -> Int
solve1 (instrs, network) = countSteps network (cycle instrs) (== "ZZZ") 0 "AAA"

solve2 :: (String, Network) -> Int
solve2 (instrs, network) = foldr lcm 1 allSteps
    where
        allStartNodes = filter ((== 'A') . last) (keys network)
        allSteps = map (countSteps network (cycle instrs) ((== 'Z') . last) 0) allStartNodes
