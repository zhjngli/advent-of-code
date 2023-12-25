module Day24.Solution (solve) where

import Data.List.Split (splitOn)
import Data.List ( tails )

solve :: IO ()
solve = do
    input <- readFile "./src/Day24/input.txt"
    putStrLn $ "2023.24.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.24.2: " ++ show (solve2BF $ parseInput input)

type Point = (Double, Double, Double)
type Vel = (Double, Double, Double)

parseInput :: String -> [(Point, Vel)]
parseInput = map pvSplit . lines
    where
        pvSplit l = case splitOn " @ " l of
            [p, v] -> (read3d p, read3d v)
            _ -> error ("can't split line: " ++ l)
        read3d vec = case splitOn ", " vec of
            [x, y, z] -> (read x, read y, read z)
            _ -> error ("can't read vec: " ++ vec)

-- calculate line: y = mx + b
calcMB :: (Point, Vel) -> (Point, Vel, Double, Double)
calcMB (p@(x, y, _), v@(dx, dy, _)) = let m = dy/dx in (p, v, m, -m * x + y)

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

solve1 :: [(Point, Vel)] -> Int
solve1 l = length $ filter (uncurry intersection) (pairs lmb)
    where lmb = map calcMB l

maxVelocity :: [(Point, Vel)] -> Double
maxVelocity = maxV' 0
    where
        maxV' n [] = n
        maxV' n ((_, (vx, vy, vz)):ls) = maxV' (maximum [n, abs vx, abs vy, abs vz]) ls

hitsStone :: (Point, Vel) -> (Point, Vel) -> Bool
hitsStone r@((x, y, z), (vx, vy, vz)) s@((sx, sy, sz), (svx, svy, svz)) =
    let t | vx /= svx = (x - sx) / (svx - vx)
          | vy /= svy = (y - sy) / (svy - vy)
          | vz /= svz = (z - sz) / (svz - vz)
          | otherwise = error ("velocities should be different: " ++ show r ++ ", " ++ show s)
    in
        x + vx * t == sx + svx * t &&
        y + vy * t == sy + svy * t &&
        z + vz * t == sz + svz * t

hitsAllStones :: (Point, Vel) -> (Point, Vel) -> Vel -> [(Point, Vel)] -> Maybe (Point, Vel)
hitsAllStones ((x1, y1, z1), (vx1, vy1, vz1)) ((x2, y2, z2), (vx2, vy2, vz2)) (vx, vy, vz) ss
    | vx == 0 || vy == 0 || vz == 0 = Nothing
    | dvs == 0 = Nothing
    | otherwise =
        let r = ((x, y, z), (vx, vy, vz)) in
        if all (hitsStone r) ss then Just r else Nothing
    where
        dx1 = vx1 - vx
        dy1 = vy1 - vy
        dx2 = vx2 - vx
        dy2 = vy2 - vy
        dvs = (dx1 * dy2) - (dx2 * dy1)
        t = (dy2 * (x2 - x1) - dx2 * (y2 - y1)) / dvs
        x = x1 + dx1 * t
        y = y1 + dy1 * t
        z = z1 + (vz1 - vz) * t

findRock :: (Point, Vel) -> (Point, Vel) -> [(Point, Vel)] -> [Vel] -> (Point, Vel)
findRock _ _ _ [] = error "couldn't find a suitable rock!"
findRock s1 s2 ss (v:vs) = case hitsAllStones s1 s2 v ss of
    Nothing -> findRock s1 s2 ss vs
    Just r -> r

solve2BF :: [(Point, Vel)] -> (Point, Vel)
solve2BF ss = findRock s1 s2 ss vs
    where
        s1 = head ss
        s2 = ss !! 1
        mv = maxVelocity ss
        vrange = [-mv..mv]
        vs = [(vx, vy, vz) |
                vx <- vrange,
                vx /= 0,
                vy <- vrange,
                vy /= 0,
                vz <- vrange,
                vz /= 0
                ]
