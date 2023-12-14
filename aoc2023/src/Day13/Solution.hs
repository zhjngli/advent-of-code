module Day13.Solution (solve) where

import Data.List.Split
import Data.List ( nub, transpose )

solve :: IO ()
solve = do
    input <- readFile "./src/Day13/input.txt"
    putStrLn $ "2023.13.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.13.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> [[String]]
parseInput i = splitOn [""] $ splitOn "\n" i

-- calculate all reflection lines
score :: [String] -> [Int]
score p = map (* 100) row ++ col
  where
      row = score' p
      col = score' $ transpose p

score' :: [String] -> [Int]
score' p = ss
    where
        ss = foldr
            (\n acc ->
                let (left, right) = splitAt n p in
                if match left right then n:acc
                else acc
            )
            []
            [1..length p-1]

match :: [String] -> [String] -> Bool
match l r
    | length l > length r = take (length r) (reverse l) == r
    | otherwise           = l == reverse (take (length l) r)

smudges :: [String] -> [[String]]
smudges p = [
    xs ++ [y'] ++ tail ys
    | n <- [0..length p - 1]
    , let (xs, ys) = splitAt n p
    , y' <- smudgeRow (head ys)
    ]
    where
        smudgeRow r = [
            xs ++ [y'] ++ tail ys
            | n <- [0..length r - 1]
            , let (xs, ys) = splitAt n r
            , let y' = smudge (head ys)
            ]
        smudge '.' = '#'
        smudge '#' = '.'
        smudge c = error ("unknown pattern: " ++ [c])

solve1 :: [[String]] -> Int
solve1 = sum . concatMap score

solve2 :: [[String]] -> Int
solve2 = sum . concatMap diffScore
    where
        diffScore :: [String] -> [Int]
        diffScore p =
            let s = head $ score p
                -- calculate all reflection lines of all smudges
                ss = concatMap score $ smudges p
            in
            -- filter out 0 scores (i.e. no reflection line)
            -- and filter out scores that match the no smudge case
            -- then use nub to uniquify
            nub $ filter (\s' -> s' /= 0 && s' /= s) ss
