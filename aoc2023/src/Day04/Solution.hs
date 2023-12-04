module Day04.Solution (solve) where

import Data.List (foldl')
import Data.Set as S (Set, fromList, intersection, size)
import Data.Map as M (elems, fromList, adjust, (!))
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day04/input.txt"
    putStrLn $ "2023.04.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.04.2: " ++ show (solve2 $ parseInput input)

parseCard :: CharParser st (Int, S.Set Int, S.Set Int)
parseCard = do
    _ <- string "Card"
    _ <- many1 (char ' ')
    cid <- many1 digit
    _ <- string ":"
    _ <- many1 (char ' ')
    wins <- endBy1 (many1 digit) (many1 $ many1 (char ' '))
    _ <- string "|"
    _ <- many1 (char ' ')
    nums <- sepBy1 (many1 digit) (many1 $ many1 (char ' '))
    return (read cid, S.fromList $ map read wins, S.fromList $ map read nums)

parseCards :: CharParser st [(Int, S.Set Int, S.Set Int)]
parseCards = endBy1 parseCard newline

parseInput :: String -> [(Int, S.Set Int, S.Set Int)]
parseInput i = case parse parseCards "" i of
    Left e -> error (show e)
    Right cs -> cs

scoreCard :: (Int, S.Set Int, S.Set Int) -> Int
scoreCard (_, w, n) = case S.size (S.intersection w n) of
    0 -> 0
    i -> 2 ^ (i - 1)

solve1 :: [(Int, S.Set Int, S.Set Int)] -> Int
solve1 = sum . map scoreCard

solve2 :: [(Int, S.Set Int, S.Set Int)] -> Int
solve2 cs = sum (M.elems totalScorecards)
    where
        numScorecards = length cs 
        totalScorecards = foldl' (
            \scorecards (cid, w, n) ->
                foldl' (
                    -- update next cid with the amount of copies of current cid
                    \scorecards' cid' -> M.adjust (+ (scorecards' M.! cid)) cid' scorecards'
                )
                scorecards
                [cid + 1 .. cid + S.size (S.intersection w n)]
            )
            (M.fromList $ zip [1..numScorecards] (repeat 1))
            cs
