module Day14.Solution (solve) where

import Data.Array
import Lib.Common
import Data.List ( foldl' )

solve :: IO ()
solve = do
    input <- readFile "./src/Day14/input.txt"
    putStrLn $ "2023.14.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.14.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> Array (Int, Int) Char
parseInput = toArray . lines

north :: (Int, Int) -> (Int, Int)
north (r, c) = (r-1, c)

west :: (Int, Int) -> (Int, Int)
west (r, c) = (r, c-1)

south :: (Int, Int) -> (Int, Int)
south (r, c) = (r+1, c)

east :: (Int, Int) -> (Int, Int)
east (r, c) = (r, c+1)

move :: ((Int, Int) -> (Int, Int)) -> Array (Int, Int) Char -> (Int, Int) -> (Int, Int)
move fi a i
    | inRange b i' && a ! i' == '.' = move fi a i'
    | otherwise = i
    where
        i' = fi i
        b = bounds a

moveAll :: (Array (Int, Int) Char -> (Int, Int) -> (Int, Int)) -> Array (Int, Int) Char -> Array (Int, Int) Char
moveAll f a = foldl'
    (\a' i ->
        if a' ! i == 'O' then let i' = f a' i in a' // [(i, '.'), (i', 'O')]
        else a'
    )
    a (indices a)

spin :: Array (Int, Int) Char -> Array (Int, Int) Char
spin a = a''''
    where
        a' = moveAll (move north) a
        a'' = moveAll (move west) a'
        a''' = moveAll (move south) a''
        a'''' = moveAll (move east) a'''

totalLoad :: Array (Int, Int) Char -> Int
totalLoad a = foldr
    (\((r', _), e) load ->
        case e of
        'O' -> load + (r + 1 - r')
        _ -> load
    )
    0 (assocs a)
    where (_, (r, _)) = bounds a

solve1 :: Array (Int, Int) Char -> Int
solve1 = totalLoad . moveAll (move north)

solve2 :: Array (Int, Int) Char -> Int
solve2 a = totalLoad $ foldr (\_ a' -> spin a') a [1::Int ..1000000000]
