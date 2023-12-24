module Day24.Solution (solve) where

import Data.List.Split (splitOn)
import Data.List ( tails )

solve :: IO ()
solve = do
    input <- readFile "./src/Day24/input.txt"
    putStrLn $ "2023.24.1: " ++ show (solve1 $ parseInput input)
    -- putStrLn $ "2023.24.2: " ++ show (solve2 $ parseInput input)

type Point = (Double, Double, Double)
type Vel = (Double, Double, Double)

parseInput :: String -> [(Point, Vel, Double, Double)]
parseInput = map pvSplit . lines
    where
        pvSplit l = case splitOn " @ " l of
            [p, v] -> calcMB (read3d p) (read3d v)
            _ -> error ("can't split line: " ++ l)
        read3d vec = case splitOn ", " vec of
            [x, y, z] -> (read x, read y, read z)
            _ -> error ("can't read vec: " ++ vec)
        calcMB p@(x, y, _) v@(dx, dy, _) = let m = dy/dx in (p, v, m, -m * x + y)

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]

pointInFuture :: (Double, Double) -> (Point, Vel) -> Bool
pointInFuture (xi, yi) ((x, y, _), (dx, dy, _)) =
    let xInFuture = if dx > 0 then xi >= x else xi <= x
        yInFuture = if dy > 0 then yi >= y else yi <= y
    in xInFuture && yInFuture

intersection :: (Point, Vel, Double, Double) -> (Point, Vel, Double, Double) -> Bool
intersection (p1, v1, m1, b1) (p2, v2, m2, b2)
    | m1 == m2 = False
    | otherwise =
        let xIntersect = (b2 - b1) / (m1 - m2)
            yIntersect = m1 * xIntersect + b1
        in
        xIntersect >= 200000000000000 && xIntersect <= 400000000000000 &&
        yIntersect >= 200000000000000 && yIntersect <= 400000000000000 &&
        pointInFuture (xIntersect, yIntersect) (p1, v1) &&
        pointInFuture (xIntersect, yIntersect) (p2, v2)

solve1 :: [(Point, Vel, Double, Double)] -> Int
solve1 l = length $ filter (uncurry intersection) (pairs l)
