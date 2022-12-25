module Day15.Solution (solve) where

-- import Debug.Trace
import Data.List
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day15/input.txt"
    putStrLn $ "2022.15.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.15.2: " ++ show (solve2 $ parseInput input)

type Sensor = (Integer, Integer)
type Beacon = (Integer, Integer)

parseSensor :: CharParser st (Sensor, Beacon)
parseSensor = do
    _ <- string "Sensor at x="
    sx <- many1 (digit <|> char '-')
    _ <- string ", y="
    sy <- many1 (digit <|> char '-')
    _ <- string ": closest beacon is at x="
    bx <- many1 (digit <|> char '-')
    _ <- string ", y="
    by <- many1 (digit <|> char '-')
    return ((read sx, read sy), (read bx, read by))

parseSensors :: CharParser st [(Sensor, Beacon)]
parseSensors = endBy1 parseSensor newline

parseInput :: String -> [(Sensor, Beacon)]
parseInput i = case parse parseSensors "" i of
    Left e -> error (show e)
    Right ss -> ss

distance :: Sensor -> Beacon -> Integer
distance (sx, sy) (bx, by) = abs (sx - bx) + abs (sy - by)

findRowCoverage :: Sensor -> Integer -> Integer -> Maybe (Integer, Integer)
findRowCoverage (sx, sy) d y = let n = d - abs (sy - y) in
    if n < 0 then Nothing
    else Just (sx - n, sx + n)

mergeCoverages :: [(Integer, Integer)] -> [(Integer, Integer)]
mergeCoverages [] = []
mergeCoverages (c:cs) = mergeCoverages' c cs

mergeCoverages' :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
mergeCoverages' c [] = [c]
mergeCoverages' c@(cmin, cmax) (c'@(cmin', cmax'):cs)
    | cmin' >= cmin && cmin' <= cmax = mergeCoverages' (cmin, max cmax' cmax) cs
    | otherwise = c:mergeCoverages' c' cs

-- the (-1) hardcodes the number of beacons in that row LOL
solve1 :: [(Sensor, Beacon)] -> Integer
solve1 sbs = foldr (\(cmin, cmax) n -> n + cmax - cmin + 1) (-1) merged
    where merged = mergeCoverages coverages
          sds = map (\(s, b) -> (s, distance s b)) sbs
          coverages = sort $ foldr f [] sds
          f (s, d) cs = case findRowCoverage s d 2000000 of
                Just c -> c:cs
                Nothing -> cs

bounded :: Integer -> Integer -> Integer -> Bool
bounded n mn mx = n <= mx && n >= mn

outOfSensorRange :: Sensor -> Integer -> [(Integer, Integer)]
outOfSensorRange (sx, sy) d = foldr f [] [1..d']
    where f n r = (sx+d'-n, sy+n):(sx-n, sy+d'-n):(sx-(d'-n), sy-n):(sx+n, sy-(d'-n)):r
          d' = d + 1

solve2 :: [(Sensor, Beacon)] -> Integer
solve2 sbs = (\(x, y) -> 4000000 * x + y) . head . map head . group . sort $ filter pointIsOutOfAllRanges outOfRangePoints
    where sds = map (\(s, b) -> (s, distance s b)) sbs
          outOfRangePoints = filter (\(x, y) -> bounded x 0 4000000 && bounded y 0 4000000) $ concatMap (uncurry outOfSensorRange) sds
          pointIsOutOfAllRanges p = all (\(s, d) -> distance s p > d) sds
