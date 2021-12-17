module Y2021.Day09.Solution where

import Data.Array
import Data.Char
import Data.List
import qualified Data.Set as S

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day09/input.txt"
    let heightMap = map (map digitToInt) (lines input)
    putStrLn $ "2021.09.1: " ++ show (solve1 heightMap)
    putStrLn $ "2021.09.2: " ++ show (solve2 heightMap)

toArray :: [[Int]] -> Array (Int, Int) Int
toArray l = array bound associations
    where r = length l
          c = length $ head l
          bound = ((0, 0), (r-1, c-1))
          cIndices = map (zip [0..c-1]) l
          rcIndices = zip [0..r-1] cIndices
          associations = concatMap associate rcIndices
          associate (r, cs) = map (\(c, i) -> ((r, c), i)) cs

neighbors :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
neighbors a (r, c) = filter (inRange (bounds a)) [(r+x, c+y) | (x, y) <- [(1,0), (-1, 0), (0, 1), (0, -1)]]

valleys :: Array (Int, Int) Int -> [((Int, Int), Int)]
valleys a = foldr valley [] (assocs a)
    where valley v@((r, c), i) vs = if all (> i) $ ns (r, c) then v:vs else vs
          ns (r, c) = map (a !) $ neighbors a (r, c)

solve1 :: [[Int]] -> Int
solve1 h = sum $ map ((+1) . snd) vs
    where a = toArray h
          vs = valleys a

findBasin :: Array (Int, Int) Int -> [(Int, Int)] -> S.Set (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findBasin a basin visited [] = basin
findBasin a basin visited ((r, c):hs) = if S.member (r, c) visited then findBasin a basin visited hs else findBasin a newBasin newVisited (hs ++ toVisit)
    where toVisit = filter (\i -> a ! i /= 9) $ neighbors a (r, c)
          newVisited = S.insert (r, c) visited
          newBasin = if a ! (r, c) /= 9 then (r, c):basin else basin

solve2 :: [[Int]] -> Int
solve2 h = basinSizes !! (l-1) * basinSizes !! (l-2) * basinSizes !! (l-3)
    where a = toArray h
          vs = map fst $ valleys a
          basinSizes = sort $ map (length . findBasin a [] S.empty . (:[])) vs
          l = length basinSizes
