module Day17.Solution (solve) where

-- import Debug.Trace
import Data.List ( foldl' )
import qualified Data.Set as S

solve :: IO ()
solve = do
    input <- readFile "./src/Day17/input.txt"
    putStrLn $ "2022.17.1: " ++ show (solve1 $ head (lines input))
    putStrLn $ "2022.17.2: " ++ show (solve2 $ head (lines input))

-- define (y, x) as coords to use max more efficiently in the set
type Rock = S.Set (Int, Int)
type Chamber = S.Set (Int, Int)
type Wind = Char

-- ####
horiz :: Rock
horiz = S.fromList $ zip [0,0,0,0] [0..3]

-- .#.
-- ###
-- .#.
cross :: Rock
cross = S.fromList [
                (2, 1),
        (1, 0), (1, 1), (1, 2),
                (0, 1)
    ]

-- ..#
-- ..#
-- ###
ell :: Rock
ell = S.fromList [
                        (2, 2),
                        (1, 2),
        (0, 0), (0, 1), (0, 2)
    ]

-- #
-- #
-- #
-- #
vert :: Rock
vert = S.fromList $ zip [0..3] [0,0,0,0]

-- ##
-- ##
block :: Rock
block = S.fromList [
        (1, 0), (1, 1),
        (0, 0), (0, 1)
    ]

rocks :: Int -> [Rock]
rocks n = concat $ replicate (n `div` l) rs ++ [take (n `mod` l) rs]
    where rs = [horiz, cross, ell, vert, block]
          l = length rs

-- map wall begins at 0,0
-- rock starts 2 units left of the wall, and 3 units above the max height
initRock :: Int -> Rock -> Rock
initRock h = S.map (\(y, x) -> (y+h+3, x+2))

blowRock :: Wind -> Rock -> Rock
blowRock '>' = S.map (\(y, x) -> (y, x+1))
blowRock '<' = S.map (\(y, x) -> (y, x-1))
blowRock c = error ("unrecognized wind " ++ [c])

fallRock :: Rock -> Rock
fallRock = S.map (\(y, x) -> (y-1, x))

canRockMove :: Chamber -> Rock -> Bool
canRockMove c r = S.null (S.intersection c r) && all (\(y, x) -> x < 7 && x >= 0 && y >= 0) (S.elems r)

dropRockStep :: (Chamber, [Wind]) -> Rock -> (Chamber, [Wind])
dropRockStep (_, []) _ = error "winds should never be empty"
dropRockStep (c, w:ws) r =
    let maybeBlownR = blowRock w r
        blownR = if canRockMove c maybeBlownR then maybeBlownR else r
        maybeDroppedR = fallRock blownR
    in
    if canRockMove c maybeDroppedR then dropRockStep (c, ws) maybeDroppedR
    else (S.union c blownR, ws)

dropRock :: (Chamber, Int, [Wind]) -> Rock -> (Chamber, Int, [Wind])
dropRock (c, h, ws) r = (c', y+1, ws')
    where initR = initRock h r
          (c', ws') = dropRockStep (c, ws) initR
          (y, _) = S.findMax c'

solve' :: Int -> [Wind] -> Int
solve' i ws = h
    where (c, h, ws') = foldl' dropRock (S.empty, 0, concat $ repeat ws) (rocks i)

solve1 :: [Wind] -> Int
solve1 = solve' 2022

solve2 :: [Wind] -> Int
solve2 = solve' 1000000000000
