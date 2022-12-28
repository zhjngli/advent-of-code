module Day17.Solution (solve) where

-- import Debug.Trace
-- import Data.List ( foldl' )
import Data.Array
import qualified Data.Set as S
import qualified Data.Map as M

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

-- prints n rows top down starting from height h
-- printChamber :: Chamber -> Int -> Int -> String
-- printChamber chamber h rows = foldl' printRow "" [0..rows]
--     where printRow s r = s ++ "\n" ++ foldr printC "" [0..6]
--             where printC c rs = if S.member (h-r, c) chamber then '#':rs else '.':rs

dropRockStep :: (M.Map Int Wind, Chamber, Int) -> Rock -> (Chamber, Int)
dropRockStep (winds, chamber, w) r =
    let wind = winds M.! (w `mod` M.size winds)
        maybeBlownR = blowRock wind r
        blownR = if canRockMove chamber maybeBlownR then maybeBlownR else r
        maybeDroppedR = fallRock blownR
    in
    if canRockMove chamber maybeDroppedR then dropRockStep (winds, chamber, w+1) maybeDroppedR
    else (S.union chamber blownR, w+1)

dropRock :: (M.Map Int Wind, M.Map Int Rock, Chamber, Int, Int, Int) -> (M.Map Int Wind, M.Map Int Rock, Chamber, Int, Int, Int)
dropRock (winds, rocks, chamber, h, w, r) = (winds, rocks, chamber', y+1, w', r+1)
    where rock = initRock h $ rocks M.! (r `mod` M.size rocks)
          (chamber', w') = dropRockStep (winds, chamber, w) rock
          (y, _) = S.findMax chamber'

-- find lowest height (or 0) in the chamber such that a rock exists at every x
chamberPattern :: Chamber -> S.Set (Int, Int)
chamberPattern chamber = foldr (\p@(py, px) s -> if S.member p chamber then S.insert (py-lowestH, px) s else s) S.empty (range ((lowestH, 0), (y, 6)))
    where (y, x) = S.findMax chamber
          lowestH = chamberPattern' y (S.singleton x)
          chamberPattern' 0 _ = 0
          chamberPattern' h xs =
                if xs == S.fromList [0..6] then h
                else let xs' = foldr (\i cols -> if S.member (h, i) chamber then S.insert i cols else cols) xs [0..6]
                     in chamberPattern' (h-1) xs'

differentState :: (M.Map Int Wind, M.Map Int Rock, Chamber, Int, Int, Int) -> (M.Map Int Wind, M.Map Int Rock, Chamber, Int, Int, Int) -> Bool
differentState (winds, rocks, c1, _, w1, r1) (_, _, c2, _, w2, r2) =
    not (chamberPattern c1 == chamberPattern c2 &&
        w1 `mod` M.size winds == w2 `mod` M.size winds &&
        r1 `mod` M.size rocks == r2 `mod` M.size rocks)

solve' :: Int -> [Wind] -> Int
solve' i ws = heightBeforeRemRocks + h - chamberHeightBeforeRemRocks
    where winds = M.fromList $ zip [0..] ws
          rocks = M.fromList $ zip [0..] [horiz, cross, ell, vert, block]
          slow = iterate dropRock (winds, rocks, S.empty, 0, 0, 0)
          fast = iterate (dropRock . dropRock) (winds, rocks, S.empty, 0, 0, 0)
          pairs = drop 1 $ zip slow fast
          ((_, _, c1, h1, w1, r1), (_, _, _, h2, _, r2)) = head $ dropWhile (uncurry differentState) pairs
          cycleHeight = h2 - h1
          cycleRocks = r2 - r1
          (numCycles, remRocks) = (i - r1) `divMod` cycleRocks
          heightBeforeRemRocks = h1 + numCycles * cycleHeight
          chamberBeforeRemRocks = chamberPattern c1
          chamberHeightBeforeRemRocks = let (y, _) = S.findMax chamberBeforeRemRocks in y
          dropRemRocks = iterate dropRock (winds, rocks, chamberBeforeRemRocks, chamberHeightBeforeRemRocks, w1, r1)
          (_, _, _, h, _, _) = head $ dropWhile (\(_, _, _, _, _, r) -> r /= r1 + remRocks) dropRemRocks

solve1 :: [Wind] -> Int
solve1 = solve' 2022

solve2 :: [Wind] -> Int
solve2 = solve' 1000000000000
