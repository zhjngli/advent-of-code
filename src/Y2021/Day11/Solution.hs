module Y2021.Day11.Solution where

import Data.Array
import Data.Char
import qualified Data.Set as S

solve = do
    input <- readFile "./src/Y2021/Day11/input.txt"
    let octopi = map (map digitToInt) (lines input)
    putStrLn $ "2021.11.1: " ++ show (solve1 octopi)
    putStrLn $ "2021.11.2: " ++ show (solve2 octopi)

-- Energy, Flashed during this step, Total flashes
data Octopus = O Int Bool Int deriving Show

toArray :: [[Int]] -> Array (Int, Int) Octopus
toArray l = array bound associations
    where r = length l
          c = length $ head l
          bound = ((0, 0), (r-1, c-1))
          cIndices = map (zip [0..c-1]) l
          rcIndices = zip [0..r-1] cIndices
          associations = concatMap associate rcIndices
          associate (r, cs) = map (\(c, i) -> ((r, c), O i False 0)) cs

neighbors :: Array (Int, Int) Octopus -> (Int, Int) -> [(Int, Int)]
neighbors a (r, c) = filter (inRange (bounds a)) [(r+x, c+y) | (x, y) <- [(1,0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]]

increaseEnergy :: Array (Int, Int) Octopus -> Array (Int, Int) Octopus
increaseEnergy = fmap inc
    where inc (O e f t) = O (e + 1) f t

flashOctopi' :: Array (Int, Int) Octopus -> [(Int, Int)] -> Array (Int, Int) Octopus
flashOctopi' a [] = a
flashOctopi' a l = flashOctopi' afterInc (shouldFlash afterInc)
    where allNeighbors = concatMap (neighbors a) l
          afterFlash = a // [(i, flash $ a ! i) | i <- l]
          afterInc = foldr incNeighbor afterFlash allNeighbors
          incNeighbor i arr = arr // [(i, inc $ arr ! i)]
          flash (O e True t) = O e True t
          flash (O e False t) = O e True (t+1)
          inc (O e f t) = O (e+1) f t

hasEnergyToFlash :: Int -> Bool
hasEnergyToFlash e = e > 9

shouldFlash :: Array (Int, Int) Octopus -> [(Int, Int)]
shouldFlash a = foldr flash [] (assocs a)
    where flash (i, O e f t) is = if hasEnergyToFlash e && not f then i:is else is

flashOctopi :: Array (Int, Int) Octopus -> Array (Int, Int) Octopus
flashOctopi a = flashOctopi' a (shouldFlash a)

resetOctopi :: Array (Int, Int) Octopus -> Array (Int, Int) Octopus
resetOctopi = fmap reset
    where reset (O e f t) = O (energy e) False t
          energy e = if hasEnergyToFlash e then 0 else e

step :: Array (Int, Int) Octopus -> Array (Int, Int) Octopus
step = resetOctopi . flashOctopi . increaseEnergy

countFlashes :: Array (Int, Int) Octopus -> Int
countFlashes = foldr count 0
    where count (O _ _ t) a = a + t

solve1 :: [[Int]] -> Int
solve1 o = countFlashes $ solve' 100 (toArray o)
    where solve' 0 a = a
          solve' x a = solve' (x-1) (step a)

solve2 :: [[Int]] -> Int
solve2 o = solve' (step a) a 1
    where a = toArray o
          size = rangeSize $ bounds a
          solve' next curr x = if countFlashes next - countFlashes curr == size then x else solve' (step next) next (x+1)
