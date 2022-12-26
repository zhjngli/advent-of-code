module Day18.Solution (solve) where

-- import Debug.Trace
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day18/input.txt"
    putStrLn $ "2022.17.8: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.17.8: " ++ show (solve2 $ parseInput input)

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

floodFill :: S.Set Point -> Point -> (S.Set Point -> Point -> [Point]) -> S.Set Point
floodFill points p ns = floodFill' points ns [p] S.empty
    where floodFill' _ _ [] f = f
          floodFill' points ns (q:qs) f = floodFill' points ns qs' (S.insert q f)
            where qs' = qs ++ ns points q

groupPoints :: S.Set Point -> [S.Set Point]
groupPoints ps
    | S.null ps = []
    | otherwise = g:groupPoints ps'
        where g = floodFill ps (S.findMax ps) (\xs x -> filter (`S.member` xs) (neighbors x))
              ps' = ps S.\\ g

solve2 :: [Point] -> [S.Set Point]
solve2 ps = groupPoints exteriors
    where pset = S.fromList ps
          exteriors = S.fromList $ concatMap (exteriorNeighbors pset) pset
