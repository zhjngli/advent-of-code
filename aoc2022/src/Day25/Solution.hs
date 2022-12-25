module Day25.Solution (solve) where

-- import Debug.Trace
import Data.List ( foldl' )

solve :: IO ()
solve = do
    input <- readFile "./src/Day25/input.txt"
    putStrLn $ "2022.25.1: " ++ solve1 (lines input)

snafuDigitVal :: Char -> Int
snafuDigitVal '2' = 2
snafuDigitVal '1' = 1
snafuDigitVal '0' = 0
snafuDigitVal '-' = -1
snafuDigitVal '=' = -2
snafuDigitVal c = error ("unknown snafu digit: " ++ [c])

fromSnafu :: [Char] -> Int
fromSnafu = foldl' (\i s -> i * 5 + snafuDigitVal s) 0

toSnafuDigit :: Int -> Char
toSnafuDigit x
    | x == 0 = '0'
    | x == 1 = '1'
    | x == 2 = '2'
    | x == 3 = '='
    | x == 4 = '-'
    | otherwise = error ("cannot convert " ++ show x ++ " to snafu digit")

toSnafu :: Int -> [Char]
toSnafu 0 = ['0']
toSnafu n = toSnafu (n' `div` 5) ++ [toSnafuDigit r]
    where r = n `mod` 5
          n' = case r of
            3 -> n + 2
            4 -> n + 1
            _ -> n

solve1 :: [[Char]] -> [Char]
solve1 cs = toSnafu . sum $ map fromSnafu cs
