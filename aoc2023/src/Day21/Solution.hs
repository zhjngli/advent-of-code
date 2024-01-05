module Day21.Solution (solve) where

import Data.Array
import Lib.Common
import qualified Data.Set as S

solve :: IO ()
solve = do
    input <- readFile "./src/Day21/input.txt"
    putStrLn $ "2023.21.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.21.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> Array (Int, Int) Char
parseInput = toArray . lines

start :: Array (Int, Int) Char -> S.Set (Int, Int)
start = S.fromList . map fst . filter ((== 'S') . snd) . assocs

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = [(r, c-1), (r, c+1), (r-1, c), (r+1, c)]

nexts :: Array (Int, Int) Char -> (Int, Int) -> S.Set (Int, Int)
nexts a ix = S.fromList ns
    where
        b = bounds a
        ns = filter
            (\(ir, ic) -> inRange b (ir, ic) && a ! (ir, ic) /= '#')
            (neighbors ix)

step :: Int -> Array (Int, Int) Char -> S.Set (Int, Int) -> S.Set (Int, Int)
step 0 _ s = s
step n a s = step (n-1) a (S.foldr' S.union S.empty (S.map (nexts a) s))

-- for printing the map if needed
-- showMap :: Array (Int, Int) Char -> S.Set (Int, Int) -> String
-- showMap a s = showArrayT a (\(i, c) -> if S.member i s then 'O' else c)

solve1 :: Array (Int, Int) Char -> Int
solve1 a = S.size s
    where s = step 64 a (start a)

{- divide the region input into 2 regions, the diamond, and the corners
each region has an odd and even case, corresponding to whether n number of steps will let you go back to the center tile
width of the whole region is 131, and the number of steps to calculate (26501365) mod 131 = 65
65 steps perfectly gets you from the start tile to the edge of the input, and it's an odd case.

define w = 131, h = 65, a = n `div` w = 202300

then there are (can figure this out by using some small cases and diagramming it):
(a+1)^2 odd diamonds
a^2 even diamonds
(a+1)a of both corner types
-}
solve2 :: Array (Int, Int) Char -> Int
solve2 arr = (a+1)^(2::Int) * oddDiamond + a^(2::Int) * evenDiamond + (a+1)*a * (oddCorner + evenCorner)
    where
        (_, (mr, _)) = bounds arr
        n = 26501365
        w = mr + 1
        h = mr `div` 2
        a = n `div` w

        st = start arr
        s65 = step 65 arr st
        s131 = step (131-65) arr s65
        s132 = step 1 arr s131

        oddDiamond = S.size s65 -- also equal to countDiamond s131
        oddCorner = S.size s131 - oddDiamond

        evenDiamond = countDiamond s132 -- also equal to S.size s64 apparently
        evenCorner = S.size s132 - evenDiamond

        countDiamond = S.foldl' (\i (r, c) -> if abs (r - h) + abs (c - h) <= h then i + 1 else i) 0
