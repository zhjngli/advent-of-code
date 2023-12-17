module Day16.Solution (solve) where

import Lib.Common (toArray)
import Data.Array
import qualified Data.Set as S

solve :: IO ()
solve = do
    input <- readFile "./src/Day16/input.txt"
    putStrLn $ "2023.16.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.16.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> Array (Int, Int) Char
parseInput = toArray . lines

data Dir = U | D | L | R deriving (Show, Eq, Ord)
type Beam = ((Int, Int), Dir)

dir :: Dir -> (Int, Int) -> (Int, Int)
dir U (r, c) = (r-1, c)
dir D (r, c) = (r+1, c)
dir L (r, c) = (r, c-1)
dir R (r, c) = (r, c+1)

shootBeam :: S.Set Beam -> Array (Int, Int) Char -> S.Set Beam -> (S.Set Beam, S.Set Beam)
shootBeam bs a m = foldr moveBeam (S.empty, m) bs
    where
        bound = bounds a
        moveBeam b@(i, d) (b', m')
            | S.member b m' = (b', m') -- already seen this beam
            | inRange bound i =
                case a ! i of
                '.' -> (S.insert (dir d i, d) b', S.insert b m')
                '\\' -> case d of
                    U -> (S.insert (dir L i, L) b', S.insert b m')
                    D -> (S.insert (dir R i, R) b', S.insert b m')
                    L -> (S.insert (dir U i, U) b', S.insert b m')
                    R -> (S.insert (dir D i, D) b', S.insert b m')
                '/' -> case d of
                    U -> (S.insert (dir R i, R) b', S.insert b m')
                    D -> (S.insert (dir L i, L) b', S.insert b m')
                    L -> (S.insert (dir D i, D) b', S.insert b m')
                    R -> (S.insert (dir U i, U) b', S.insert b m')
                '-' -> case d of
                    U -> (S.insert (dir L i, L) $ S.insert (dir R i, R) b', S.insert b m')
                    D -> (S.insert (dir L i, L) $ S.insert (dir R i, R) b', S.insert b m')
                    L -> (S.insert (dir L i, L) b', S.insert b m')
                    R -> (S.insert (dir R i, R) b', S.insert b m')
                '|' -> case d of
                    U -> (S.insert (dir U i, U) b', S.insert b m')
                    D -> (S.insert (dir D i, D) b', S.insert b m')
                    L -> (S.insert (dir U i, U) $ S.insert (dir D i, D) b', S.insert b m')
                    R -> (S.insert (dir U i, U) $ S.insert (dir D i, D) b', S.insert b m')
                c -> error ("unknown char at " ++ show i ++ ": " ++ [c])
            | otherwise = (b', m')

iterateBeams :: Array (Int, Int) Char -> S.Set Beam -> S.Set Beam -> S.Set Beam
iterateBeams a bs m
    | S.null bs = m
    | otherwise = iterateBeams a bs' m'
    where (bs', m') = shootBeam bs a m

energize :: S.Set Beam -> Int
energize = S.size . S.fromList . map fst . S.elems

solve1 :: Array (Int, Int) Char -> Int
solve1 a = energize allBeams
    where
        initBeam = ((0, 0), R)
        allBeams = iterateBeams a (S.singleton initBeam) S.empty

solve2 :: Array (Int, Int) Char -> Int
solve2 a = maximum $ map energize allConfigs
    where
        ((minr, minc), (maxr, maxc)) = bounds a
        initBeams =
            [((r, minc), R) | r <- [minr..maxr]] ++
            [((l, maxc), L) | l <- [minr..maxr]] ++
            [((maxr, u), U) | u <- [minc..maxc]] ++
            [((minr, d), D) | d <- [minc..maxc]]
        allConfigs = map (\b -> iterateBeams a (S.singleton b) S.empty) initBeams
