module Day12.Solution (solve) where

import Data.List.Split
import Data.List ( group, intercalate )

solve :: IO ()
solve = do
    input <- readFile "./src/Day12/input.txt"
    putStrLn $ "2023.12.1: " ++ show (solve' 1 $ parseInput input)
    putStrLn $ "2023.12.2: " ++ show (solve' 5 $ parseInput input)

data Condition = O | D | U deriving (Show, Eq)
type Spring = [Condition]

charToCondition :: Char -> Condition
charToCondition '.' = O
charToCondition '#' = D
charToCondition '?' = U
charToCondition c = error ("unknown condition: " ++ [c])

parseInput :: String -> [(Spring, [Int])]
parseInput i = map parseRow (lines i)
    where parseRow r = (map charToCondition (head s), map read (splitOn "," (last s)))
            where s = splitOn " " r -- assumes s only has two elements

springToPossibilities :: Spring -> [Spring]
springToPossibilities s =
    let (xs, ys) = break (== U) s in
    case ys of
    [] -> [xs]
    _ -> [
        xs ++ [p] ++ ss
        | let possibilities =
                case head ys of
                    D -> [D]
                    O -> [O]
                    U -> [D, O]
        , p <- possibilities
        , ss <- springToPossibilities (tail ys)
        ]

springToDamageGroups :: Spring -> [Int]
springToDamageGroups s = map length $ filter (all (== D)) (group s)

unfold :: Int -> (Spring, [Int]) -> (Spring, [Int])
unfold n (s, is) = (intercalate [U] (replicate n s), concat $ replicate n is)

solve' :: Int -> [(Spring, [Int])] -> Int
solve' n springs = sum $ map ((\(s, is) -> length $ filter ((== is) . springToDamageGroups) (springToPossibilities s)) . unfold n) springs
