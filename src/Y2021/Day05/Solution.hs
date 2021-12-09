module Y2021.Day05.Solution where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (Line)
import GHC.Conc (pseq)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day05/input.txt"
    putStrLn $ "2021.05.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.05.2: " ++ show (solve2 $ parseInput input)

data Point = P Int Int deriving (Show, Eq, Ord)
data Line = L Point Point deriving Show

parseLine :: CharParser st Line
parseLine = do
    x1 <- many1 digit
    char ','
    y1 <- many1 digit
    char ' '
    char '-'
    char '>'
    char ' '
    x2 <- many1 digit
    char ','
    y2 <- many1 digit
    return $ L (P (read x1) (read y1)) (P (read x2) (read y2))

parseLines :: CharParser st [Line]
parseLines = endBy parseLine newline

parseInput :: String -> [Line]
parseInput i = case parse parseLines "" i of
    Left e -> []
    Right lines -> lines

coveredPoints :: [Line] -> [Point]
coveredPoints = foldr cover []
    where cover (L (P x1 y1) (P x2 y2)) ps
            | x1 == x2 = addY x1 ps (min y1 y2) (max y1 y2)
            | y1 == y2 = addX y1 ps (min x1 x2) (max x1 x2)
            | otherwise = ps
          addY x ps y0 y
            | y == y0 = P x y : ps
            | otherwise = P x y : addY x ps y0 (y - 1)
          addX y ps x0 x
            | x == x0 = P x y : ps
            | otherwise = P x y : addX y ps x0 (x - 1)

coveredPointsWithDiagonals :: [Line] -> [Point]
coveredPointsWithDiagonals = foldr cover []
    where cover (L (P x1 y1) (P x2 y2)) ps
            | x1 == x2 = addY x1 ps (min y1 y2) (max y1 y2)
            | y1 == y2 = addX y1 ps (min x1 x2) (max x1 x2)
            | x1 < x2 && y1 < y2 = addD (+1) (+1) x1 y1 x2 y2 ps
            | x1 < x2 && y1 > y2 = addD (+1) (subtract 1) x1 y1 x2 y2 ps
            | x1 > x2 && y1 < y2 = addD (subtract 1) (+1) x1 y1 x2 y2 ps
            | x1 > x2 && y1 > y2 = addD (subtract 1) (subtract 1) x1 y1 x2 y2 ps
            | otherwise = error "unrecognized line case"
          addY x ps y0 y
            | y == y0 = P x y : ps
            | otherwise = P x y : addY x ps y0 (y - 1)
          addX y ps x0 x
            | x == x0 = P x y : ps
            | otherwise = P x y : addX y ps x0 (x - 1)
          addD fx fy x y x0 y0 ps
            | x == x0 && y == y0 = P x y : ps
            | otherwise = P x y : addD fx fy (fx x) (fy y) x0 y0 ps

countCoveredPoints :: [Point] -> M.Map Point Int
countCoveredPoints = foldr count M.empty
    where count p m = if M.member p m then M.adjust (+1) p m else M.insert p 1 m

countOverlaps :: M.Map Point Int -> Int
countOverlaps = M.foldr (\a c -> if a >= 2 then c + 1 else c) 0

solve1 :: [Line] -> Int
solve1 = countOverlaps . countCoveredPoints . coveredPoints

solve2 :: [Line] -> Int
solve2 = countOverlaps . countCoveredPoints . coveredPointsWithDiagonals
