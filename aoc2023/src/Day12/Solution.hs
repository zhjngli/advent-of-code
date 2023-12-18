module Day12.Solution (solve) where

import Data.List.Split ( splitOn )
import Data.List ( intercalate )
import Data.MemoTrie ( memoFix )

solve :: IO ()
solve = do
    input <- readFile "./src/Day12/input.txt"
    putStrLn $ "2023.12.1: " ++ show (solve' 1 $ parseInput input)
    putStrLn $ "2023.12.2: " ++ show (solve' 5 $ parseInput input)

parseInput :: String -> [(String, [Int])]
parseInput i = map parseRow (lines i)
    where parseRow r = (head s, map read (splitOn "," (last s)))
            where s = splitOn " " r -- assumes s only has two elements

unfold :: Int -> (String, [Int]) -> (String, [Int])
unfold n (s, is) = (intercalate "?" (replicate n s), concat $ replicate n is)

-- (spring representation, damaged groups) -> number of possibilities
numPossibilities :: (String, [Int]) -> Int
numPossibilities = memoFix count
  where
    count _ ([], []) = 1
    count _ ([], _:_) = 0
    count _ (s, []) = if '#' `notElem` s then 1 else 0
    count f ('.':s, ns) = f (s, ns)
    count f ('?':s, ns) = f ('.':s, ns) + f ('#':s, ns)
    count f ('#':s, n:ns) =
        let (damaged, end) = splitAt (n-1) s in
        if '.' `notElem` damaged && length damaged == n-1 then
            case end of
            [] -> f ([], ns)
            '#':_ -> 0
            '.':e -> f (e, ns)
            '?':e -> f (e, ns)
            c:_ -> error ("unknown condition: " ++ [c])
        else 0
    count _ (c:_, _) = error ("unknown condition: " ++ [c])

solve' :: Int -> [(String, [Int])] -> Int
solve' n springs = sum $ map (numPossibilities . unfold n) springs


-- brute force method but i'm keeping it cause i like the list comprehension
-- springToPossibilities :: String -> [String]
-- springToPossibilities s =
--     let (xs, ys) = break (== '?') s in
--     case ys of
--     [] -> [xs]
--     _ -> [
--         xs ++ [p] ++ ss
--         | let possibilities =
--                 case head ys of
--                     '#' -> "#"
--                     '.' -> "."
--                     '?' -> ".#"
--                     c -> error ("unkown condition: " ++ [c])
--         , p <- possibilities
--         , ss <- springToPossibilities (tail ys)
--         ]

-- springToDamageGroups :: String -> [Int]
-- springToDamageGroups s = map length $ filter (all (== '#')) (group s)

-- solveBruteForce :: Int -> [(String, [Int])] -> Int
-- solveBruteForce n springs = sum $ map ((\(s, is) -> length $ filter ((== is) . springToDamageGroups) (springToPossibilities s)) . unfold n) springs


-- just for future reference, if i wanted to use my own data type representation for the springs
-- i'd need to create HasTrie instance of the type representation and derive it from generics

-- {-# LANGUAGE DeriveGeneric, TypeFamilies, TypeOperators #-}
-- import Data.MemoTrie
--     ( enumerateGeneric,
--       memoFix,
--       trieGeneric,
--       untrieGeneric,
--       HasTrie(..),
--       Reg )
-- import GHC.Generics

-- data Condition = O | D | U deriving (Show, Eq, Generic)
-- instance HasTrie Condition where
--     newtype  Condition :->: b = ConditionTrie { unConditionTrie :: Reg Condition :->: b }
--     trie = trieGeneric ConditionTrie
--     untrie = untrieGeneric unConditionTrie
--     enumerate = enumerateGeneric unConditionTrie

-- charToCondition :: Char -> Condition
-- charToCondition '.' = O
-- charToCondition '#' = D
-- charToCondition '?' = U
-- charToCondition c = error ("unknown condition: " ++ [c])
