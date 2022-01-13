module Y2021.Day19.Solution where

import Data.Bifunctor
import Data.List
import qualified Data.Set as S
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day19/input.txt"
    putStrLn $ "2021.19.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.19.2: " ++ show (solve2 $ parseInput input)

type Loc = (Int, Int, Int)

newtype Transform = T (Loc -> Loc)

instance Semigroup Transform where
    (T a) <> (T b) = T (a . b)

-- this is highly questionable
instance Eq Transform where
    (T a) == (T b) = let l = (1, 2, 3) in a l == b l

instance Show Transform where
    show (T t) = show (1, 2, 3) ++ " -> " ++ show (t (1, 2, 3))

data Scanner = S { beacons :: [Loc], distances :: [Int], transform :: Transform } deriving (Show, Eq)

idT :: Transform
idT = T id

translate :: Loc -> Transform
translate (x, y, z) = T (\(a, b, c) -> (a+x, b+y, c+z))

rotateX :: Transform
rotateX = T (\(x, y, z) -> (x, -z, y))

rotateY :: Transform
rotateY = T (\(x, y, z) -> (z, y, -x))

rotateZ :: Transform
rotateZ = T (\(x, y, z) -> (-y, x, z))

rotations :: [Transform]
rotations = [a <> b | a <- f, b <- s]
    where f = [idT, rotateX, rotateX <> rotateX, rotateX <> rotateX <> rotateX,
                      rotateY, rotateY <> rotateY <> rotateY]
          s = [idT, rotateZ, rotateZ <> rotateZ, rotateZ <> rotateZ <> rotateZ]

parseLoc :: CharParser st Loc
parseLoc = do
    x <- many1 (digit <|> char '-')
    char ','
    y <- many1 (digit <|> char '-')
    char ','
    z <- many1 (digit <|> char '-')
    return (read x, read y, read z)

parseScanner :: CharParser st Scanner
parseScanner = do
    string "--- scanner "
    many1 digit
    string " ---"
    newline
    beacons <- endBy1 parseLoc newline
    return $ S beacons (pairwiseDistances beacons) idT

parseScanners :: CharParser st [Scanner]
parseScanners = sepBy1 parseScanner newline

parseInput :: String -> [Scanner]
parseInput i = case parse parseScanners "" i of
    Left e -> error (show e)
    Right s -> s

pairwiseDistances :: [Loc] -> [Int]
pairwiseDistances bs = [(x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2 | a@(x1, y1, z1) <- bs, b@(x2, y2, z2) <- bs, a < b]

distancesMatched :: Scanner -> Scanner -> Int
distancesMatched s1 s2 = length [d1 | d1 <- d1s, d2 <- d2s, d1 == d2]
    where d1s = distances s1
          d2s = distances s2

transformBetween :: Scanner -> Scanner -> [Transform]
transformBetween s1 s2 =
    [ t <> rot
    | let b1s = beacons s1
    , let b2s = beacons s2
    , rot@(T r) <- rotations
    , b1@(i, j, k) <- b1s
    , b2 <- b2s
    , let (x, y, z) = r b2
    , let t = translate (i-x, j-y, k-z)
    , let b2s' = let (T transform) = t <> rot in map transform b2s
    , let beaconMatches = length [a | a <- b1s, b <- b2s', a == b]
    , beaconMatches >= 12
    ]

transformScanners :: Scanner -> [Scanner] -> ([Scanner], [Scanner])
transformScanners s remaining = (transformed, remaining')
    where candidates = filter ((>= 66) . distancesMatched s) remaining  -- 66 = 12 * 11 / 2 (aiming for 12 matched beacons)
          candidatesWithTransform = map (first head) $ filter (not . null . fst) $ map (\c -> (transformBetween s c, c)) candidates
          remaining' = remaining \\ map snd candidatesWithTransform
          transformed = map (\(T t, S b d _) -> S (map t b) d (T t)) candidatesWithTransform

transformAllScanners :: [Scanner] -> [Scanner]
transformAllScanners [] = []
transformAllScanners (s:ss) = transAll [] [s] ss
    where transAll done [] _ = done
          transAll done (w:working) remaining =
              let (transformed, remaining') = transformScanners w remaining in
              transAll (w:done) (working ++ transformed) remaining'

solve1 :: [Scanner] -> Int
solve1 s = S.size $ S.unions $ map (S.fromList . beacons) $ transformAllScanners s

solve2 :: [Scanner] -> Int
solve2 s = maximum manhattanDistances
    where s' = transformAllScanners s
          slocs = map (\(S _ _ (T t)) -> t (0, 0, 0)) s'
          manhattanDistances = [(x2-x1) + (y2-y1) + (z2-z1) | a@(x1, y1, z1) <- slocs, b@(x2, y2, z2) <- slocs, a < b]
