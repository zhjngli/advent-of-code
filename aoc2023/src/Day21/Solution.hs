module Day21.Solution (solve) where

import Data.Array
import Lib.Common
import qualified Data.Set as S
import Debug.Trace

solve :: IO ()
solve = do
    input <- readFile "./src/Day21/input.txt"
    putStrLn $ "2023.21.1: " ++ show (solve1 $ parseInput input)
    -- putStrLn $ "2023.21.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> Array (Int, Int) Char
parseInput = toArray . lines

start :: Array (Int, Int) Char -> S.Set (Int, Int)
start = S.fromList . map fst . filter ((== 'S') . snd) . assocs

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = [(r, c-1), (r, c+1), (r-1, c), (r+1, c)]

nexts :: Array (Int, Int) Char -> (Int, Int) -> S.Set (Int, Int)
nexts a ix = S.fromList ns
    where
        (_, (br, bc)) = bounds a
        ns = filter
            (\(ir, ic) -> a ! (ir `mod` br, ic `mod` bc) /= '#')
            (neighbors ix)

step :: Int -> Array (Int, Int) Char -> S.Set (Int, Int) -> S.Set (Int, Int)
step 0 _ s = s
step n a s = step (n-1) a (S.foldr' S.union S.empty (S.map (nexts a) s))

showMap :: Array (Int, Int) Char -> S.Set (Int, Int) -> String
showMap a s = showArrayT a (\(i, c) -> if S.member i s then 'O' else c)

solve1 :: Array (Int, Int) Char -> Int
solve1 a = trace ("ARRAY AT 64:" ++ show (S.size s) ++ "\n" ++ showMap a s)
    S.size s
    where
        s = step 64 a (start a)
