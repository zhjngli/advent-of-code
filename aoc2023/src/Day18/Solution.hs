module Day18.Solution (solve) where

import Data.List (foldl')
import Data.List.Split
import Data.Text.Internal.Read (hexDigitToInt)

solve :: IO ()
solve = do
    input <- readFile "./src/Day18/input.txt"
    putStrLn $ "2023.18.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.18.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> [(Char, Int, String)]
parseInput = map parse . lines
    where
        parse l = case splitOn " " l of
            [d, n, hex] -> (head d, read n, hex)
            _ -> error ("can't parse line: " ++ l)

data Dir = U | D | L | R deriving (Show, Eq)

readCodes :: [(Char, Int, String)] -> [(Dir, Int)]
readCodes = map r
    where
        r (_, _, s) = let s' = init $ drop 2 s in (charToDir $ last s', foldl' (\n c -> hexDigitToInt c + 16 * n) 0 (init s'))
        charToDir '0' = R
        charToDir '1' = D
        charToDir '2' = L
        charToDir '3' = U
        charToDir c = error ("unknown direction: " ++ [c])

readSimple :: [(Char, Int, String)] -> [(Dir, Int)]
readSimple = map r
    where
        r (c, n, _) = (charToDir c, n)
        charToDir 'R' = R
        charToDir 'D' = D
        charToDir 'L' = L
        charToDir 'U' = U
        charToDir c = error ("unknown direction: " ++ [c])

vertices :: [(Dir, Int)] -> [(Int, Int)]
vertices = foldl' (\acc (d, n) -> let nix = next d (head acc) n in nix:acc) [(0,0)]
    where
        next d (r, c) n = case d of
            U -> (r-n, c)
            D -> (r+n, c)
            L -> (r, c-n)
            R -> (r, c+n)

area :: [(Int, Int)] -> Int
area vs = abs insideArea `div` 2 + len `div` 2 + 1 -- len/2 + 1 calculates the area of the "edge"
    where
        (insideArea, len, _) = foldl' (\(a, l, ix1) ix2 -> (a + ar ix1 ix2, l + d ix1 ix2, ix2)) (0, 0, head vs) (tail vs)
        d (r1, c1) (r2, c2) = abs (r1 - r2 + c1 - c2)
        ar (r1, c1) (r2, c2) = (r1 + r2) * (c1 - c2)

solve1 :: [(Char, Int, String)] -> Int
solve1 = area . vertices . readSimple

solve2 :: [(Char, Int, String)] -> Int
solve2 = area . vertices . readCodes
