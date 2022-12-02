module Y2021.Day03.Solution where

import Data.List
import Data.Char

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day03/input.txt"
    let bits = lines input
    putStrLn $ "2021.03.1: " ++ show (solve1 bits)
    putStrLn $ "2021.03.2: " ++ show (solve2 bits)

bitsToDec :: [Int] -> Int
bitsToDec = foldl' (\s b -> s * 2 + b) 0

solve1 :: [[Char]] -> Int
solve1 bits = bitsToDec mostCommonBits * bitsToDec leastCommonBits
    where rotateRight = transpose . reverse
          rotatedBits = rotateRight bits
          ones = foldr (\b a -> if b == '1' then a + 1 else a) 0
          mostCommonBit b = if ones b > (length b `div` 2) then 1 else 0
          leastCommonBit b = if ones b > (length b `div` 2) then 0 else 1
          mostCommonBits = map mostCommonBit rotatedBits
          leastCommonBits = map leastCommonBit rotatedBits

solve2 :: [[Char]] -> Int
solve2 bits = bitsToDec o2rating * bitsToDec co2rating
    where o2rating = map digitToInt $ filterBits bits (\bs i c -> mostCommonBit bs i == c) 0
          co2rating = map digitToInt $ filterBits bits (\bs i c -> leastCommonBit bs i == c) 0

ones :: [[Char]] -> Int -> Int
ones bits i = foldr (\bs a -> if bs!!i == '1' then a + 1 else a) 0 bits

mostCommonBit :: [[Char]] -> Int -> Char
mostCommonBit bits i = if o >= (length bits - o) then '1' else '0'
    where o = ones bits i

leastCommonBit :: [[Char]] -> Int -> Char
leastCommonBit bits i = if o >= (length bits - o) then '0' else '1'
    where o = ones bits i

filterBits :: [[Char]] -> ([[Char]] -> Int -> Char -> Bool) -> Int -> [Char]
filterBits [b] _ _ = b
filterBits bits cond i = filterBits nextBits cond (i + 1)
    where nextBits = foldr (\b a -> if cond bits i $ b!!i then b:a else a) [] bits
