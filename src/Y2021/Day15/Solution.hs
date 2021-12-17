module Y2021.Day15.Solution where

import Data.Char
import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day15/input.txt"
    let riskLevel = map (map digitToInt) (lines input)
    putStrLn $ "2021.15.1: " ++ show (solve1 riskLevel)
    putStrLn $ "2021.15.2: " ++ show (solve2 riskLevel)

data Q = Q Int (Int, Int) deriving (Show, Eq, Ord)

toArray :: [[Int]] -> Array (Int, Int) Int
toArray l = array bound associations
    where r = length l
          c = length $ head l
          bound = ((0, 0), (r - 1, c - 1))
          cIndices = map (zip [0..c-1]) l
          rcIndices = zip [0..r-1] cIndices
          associations = concatMap associate rcIndices
          associate (r, cs) = map (\(c, i) -> ((r, c), i)) cs

-- i originally thought the problem only allowed going down and right, and this surprisingly worked for the simple cases
-- downRightNeighbors :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
-- downRightNeighbors b (r, c) = filter (inRange b) [(r+x, c+y) | (x, y) <- [(-1, 0), (0, -1)]]

-- calcPath :: Array (Int, Int) Int -> Int
-- calcPath a = calcPath' 0 a ! (maxX, maxY) - a ! (0, 0)
--     where ((_, _), (maxX, maxY)) = bounds a
--           calcPathX x y arr | x <= maxX = calcPathX (x+1) y a'
--                             | otherwise = arr
--                               where a' = arr // [((x, y), v)]
--                                     prev = [arr ! (i, j) | (i, j) <- downRightNeighbors (bounds arr) (x, y)]
--                                     cost = if not $ null prev then minimum prev else 0
--                                     v = (arr ! (x, y)) + cost
--           calcPath' y arr | y <= maxY = calcPath' (y+1) $ calcPathX 0 y arr
--                           | otherwise = arr

-- although creating the multiplied array isn't inefficient, updating the multiplied array n times is
-- multArray :: Int -> Array (Int, Int) Int -> Array (Int, Int) Int
-- multArray n a = array newBound $! map f $! range newBound
--     where ((_, _), (i, j)) = bounds a
--           r = i + 1
--           c = j + 1
--           newBound = ((0, 0), (r * n - 1, c * n - 1))
--           f (x, y) = ((x, y), calcV distFromOrig $! a ! (origX, origY))
--               where calcV 0 v = v
--                     calcV d v = calcV (d-1) $! if v+1 > 9 then 1 else v+1
--                     distX = x `div` c
--                     distY = y `div` r
--                     distFromOrig = distX + distY
--                     origX = x `mod` c
--                     origY = y `mod` r

neighbors :: ((Int, Int), (Int, Int)) -> (Int, Int) -> [(Int, Int)]
neighbors b (r, c) = filter (inRange b) [(r+x, c+y) | (x, y) <- [(1, 0), (0, 1), (-1, 0), (0, -1)]]

inf :: Int
inf = 999999999

getCost :: Array (Int, Int) Int -> (Int, Int) -> Int
getCost a (x, y) = calcV distFromOrig $ a ! (origX, origY)
    where ((_, _), (i, j)) = bounds a
          r = i + 1
          c = j + 1
          calcV 0 v = v
          calcV d v = calcV (d-1) $ if v+1 > 9 then 1 else v+1
          distX = x `div` c
          distY = y `div` r
          distFromOrig = distX + distY
          origX = x `mod` c
          origY = y `mod` r

-- pred unused but i could theoretically reconstruct the path
dijkstras :: Array (Int, Int) Int -> ((Int, Int), (Int, Int)) -> M.Map (Int, Int) Int -> M.Map (Int, Int) (Int, Int) -> S.Set Q -> M.Map (Int, Int) Int
dijkstras orig b dist pred q
    | S.null q = dist
    | otherwise = dijkstras orig b dist' pred' q''
        where (Q u (ux, uy), q') = S.deleteFindMin q
              uns = neighbors b (ux, uy)
              (dist', pred', q'') = foldr funs (dist, pred, q') uns
              funs (vx, vy) (d', p', queue) = (d'', p'', queue')
                  where alt = M.findWithDefault inf (ux, uy) d' + getCost orig (vx, vy)
                        distV = M.findWithDefault inf (vx, vy) d'
                        (d'', p'', queue') | alt < distV = (M.insert (vx, vy) alt d', M.insert (vx, vy) (ux, uy) p', S.insert (Q alt (vx, vy)) queue)
                                           | otherwise = (d', p', queue)

solveXArray :: Int -> [[Int]] -> Int
solveXArray n h = M.findWithDefault inf (r, c) $ dijkstras a b dist pred queue
    where a = toArray h
          origB@(_, (origR, origC)) = bounds a
          b@(_, (r, c)) = ((0, 0), ((origR+1) * n - 1, (origC+1) * n - 1))
          (dist, queue) = foldr f (M.empty, S.empty) $ range b
          f (0, 0) (d, q) = (M.insert (0, 0) 0   d, S.insert (Q 0   (0, 0)) q)
          f (x, y) (d, q) = (M.insert (x, y) inf d, S.insert (Q inf (x, y)) q)
          pred = M.empty

solve1 :: [[Int]] -> Int
solve1 = solveXArray 1

solve2 :: [[Int]] -> Int
solve2 = solveXArray 5
