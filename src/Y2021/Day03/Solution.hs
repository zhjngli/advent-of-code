module Y2021.Day03.Solution where

import Data.List

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day03/input.txt"
    let bits = lines input
    putStrLn $ "2021.03.1: " ++ show (solve1 bits)

solve1 :: [[Char]] -> Int
solve1 bits = bitsToDec mostCommonBits * bitsToDec leastCommonBits
    where rotateRight = transpose . reverse
          rotatedBits = rotateRight bits
          ones = foldr (\b a -> if b == '1' then a + 1 else a) 0
          mostCommonBit b = if ones b > (length b `div` 2) then 1 else 0
          leastCommonBit b = if ones b > (length b `div` 2) then 0 else 1
          mostCommonBits = map mostCommonBit rotatedBits
          leastCommonBits = map leastCommonBit rotatedBits
          bitsToDec = foldl' (\s b -> s * 2 + b) 0
