module Day18.Solution (solve) where

-- import Debug.Trace
import Data.Array
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day18/input.txt"
    putStrLn $ "2022.18.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.18.2: " ++ show (solve2 $ parseInput input)

type Point = (Int, Int, Int)

parsePoint :: CharParser st Point
parsePoint = do
    x <- many1 digit
    _ <- char ','
    y <- many1 digit
    _ <- char ','
    z <- many1 digit
    return (read x, read y, read z)

parsePoints :: CharParser st [Point]
parsePoints = endBy1 parsePoint newline

parseInput :: String -> [Point]
parseInput i = case parse parsePoints "" i of
    Left e -> error (show e)
    Right ps -> ps

neighbors :: Point -> [Point]
neighbors (x, y, z) = [(x+dx, y, z) | dx <- [-1, 1]] ++ [(x, y+dy, z) | dy <- [-1, 1]] ++ [(x, y, z+dz) | dz <- [-1, 1]]

exteriorNeighbors :: S.Set Point -> Point -> [Point]
exteriorNeighbors ps p = filter (`S.notMember` ps) (neighbors p)

facesShowing :: S.Set Point -> Point -> Int
facesShowing ps p = length $ exteriorNeighbors ps p

solve1 :: [Point] -> Int
solve1 ps = sum $ map (facesShowing pset) ps
    where pset = S.fromList ps

floodFill :: S.Set Point -> Point -> ((Int, Int, Int), (Int, Int, Int)) -> (S.Set Point -> Point -> [Point]) -> S.Set Point
floodFill points p b neighborsFun = floodFill' points neighborsFun [p] S.empty
    where floodFill' _ _ [] f = f
          floodFill' ps ns (q:qs) f = floodFill' ps ns qs' (S.insert q f)
            where qs' = qs ++ filter (\x -> inRange b x && S.notMember x f && S.notMember x (S.fromList qs)) (ns ps q)

getBounds :: S.Set Point -> ((Int, Int, Int), (Int, Int, Int))
getBounds ps = (minb, (mx+1, my+1, mz+1))
    where minb = (-1, -1, -1)
          (mx, my, mz) = S.foldr f minb ps
          f (x, y, z) (bx, by, bz) = (
                if x > bx then x else bx,
                if y > by then y else by,
                if z > bz then z else bz
            )

solve2 :: [Point] -> Int
solve2 ps = sum $ map (facesShowing totalSet) total
    where pset = S.fromList ps
          b = getBounds pset
          exterior = floodFill pset (fst b) b exteriorNeighbors
          airPockets = foldr (\x l -> if S.member x pset || S.member x exterior then l else x:l) [] (range b)
          total = ps ++ airPockets
          totalSet = S.fromList total
