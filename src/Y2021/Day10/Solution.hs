module Y2021.Day10.Solution where

import Data.List
import qualified Data.Set as S

solve = do
    input <- readFile "./src/Y2021/Day10/input.txt"
    let syntaxLines = lines input
    putStrLn $ "2021.10.1: " ++ show (solve1 syntaxLines)
    putStrLn $ "2021.10.2: " ++ show (solve2 syntaxLines)

data LineStatus = Corrupted Char | Incomplete String

openBrackets :: S.Set Char
openBrackets = S.fromList "{[(<"

closeBrackets :: S.Set Char
closeBrackets = S.fromList "}])>"

match :: Char -> Char -> Bool
match '{' '}' = True
match '[' ']' = True
match '(' ')' = True
match '<' '>' = True
match _ _ = False

findLineStatus :: String -> String -> LineStatus
findLineStatus s [] = Incomplete s
findLineStatus [] (c:cs) =
    if S.member c openBrackets then findLineStatus [c] cs
    else error "closing brackets on unopened stack"
findLineStatus stack@(s:ss) (c:cs)
    | match s c = findLineStatus ss cs
    | S.member s openBrackets && S.member c closeBrackets = Corrupted c
    | otherwise = findLineStatus (c:stack) cs

scoreCorruptedLine :: LineStatus -> Int
scoreCorruptedLine (Corrupted ')') = 3
scoreCorruptedLine (Corrupted ']') = 57
scoreCorruptedLine (Corrupted '}') = 1197
scoreCorruptedLine (Corrupted '>') = 25137
scoreCorruptedLine _ = 0

solve1 :: [String] -> Int
solve1 ls = sum $ map (scoreCorruptedLine . findLineStatus []) ls

scoreIncompleteLine :: LineStatus -> Int
scoreIncompleteLine (Incomplete s) = foldl' score 0 s
    where score a '(' = 5 * a + 1
          score a '[' = 5 * a + 2
          score a '{' = 5 * a + 3
          score a '<' = 5 * a + 4
          score a _ = a
scoreIncompleteLine _ = 0

solve2 :: [String] -> Int
solve2 ls = scores !! mid
    where scores = sort $ filter (/= 0) $ map (scoreIncompleteLine . findLineStatus []) ls
          mid = length scores `div` 2
