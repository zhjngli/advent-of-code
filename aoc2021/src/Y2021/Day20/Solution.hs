module Y2021.Day20.Solution where

import Data.Array
import Data.List
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day20/input.txt"
    putStrLn $ "2021.20.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.20.2: " ++ show (solve2 $ parseInput input)

data ImgAlgo = IAlgo { algo :: [Int],
                       first :: Int,
                       last :: Int } deriving Show
data Img = Img { img :: Array (Int, Int) Int,
                 outOfBoundValue :: Int } deriving Show

parseDark :: CharParser st Int
parseDark = do
    char '.'
    return 0

parseLight :: CharParser st Int
parseLight = do
    char '#'
    return 1

parseLine :: CharParser st [Int]
parseLine = many1 (parseLight <|> parseDark)

parseAlgo :: CharParser st ImgAlgo
parseAlgo = do
    f <- parseLight <|> parseDark
    i <- count 510 (parseLight <|> parseDark)
    l <- parseLight <|> parseDark
    newline
    return $ IAlgo (f:i ++ [l]) f l

parseLines :: CharParser st (ImgAlgo, Img)
parseLines = do
    algo <- parseAlgo
    newline
    img <- endBy1 parseLine newline
    return (algo, toImg img)

parseInput :: String -> (ImgAlgo, Img)
parseInput i = case parse parseLines "" i of
    Left e -> error (show e)
    Right i -> i

toImg :: [[Int]] -> Img
toImg l = Img (array bound associations) 0
    where r = length l
          c = length $ head l
          bound = ((0, 0), (r-1, c-1))
          cIndices = map (zip [0..c-1]) l
          rcIndices = zip [0..r-1] cIndices
          associations = concatMap associate rcIndices
          associate (r, cs) = map (\(c, i) -> ((r, c), i)) cs

bitsToDec :: [Int] -> Int
bitsToDec = foldl' (\s b -> s * 2 + b) 0

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = [(r+x, c+y) | (x, y) <- [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 0), (0, 1), (1, -1), (1, 0), (1, 1)]]

getVal :: Img -> (Int, Int) -> Int
getVal (Img i v) xy | inRange (bounds i) xy = i ! xy
                    | otherwise = v

enhance :: ImgAlgo -> Img -> Img
enhance (IAlgo a f l) img@(Img i v) = Img i' v'
    where b = let ((m, n), (l, p)) = bounds i in ((m-1, n-1), (l+1, p+1))
          v' | v == 0 && f == 1 = 1
             | v == 1 && l == 0 = 0
             | otherwise = v
          i' = array b assocs
          assocs = foldr f [] (range b)
              where f xy l = (xy, a !! (bitsToDec . map (getVal img) $ neighbors xy)):l

solve' :: Int -> (ImgAlgo, Img) -> Int
solve' n (ia, i) = foldr (\(_, v) x -> if v == 1 then x+1 else x) 0 (assocs i')
    where (Img i' _) = iterate (enhance ia) i !! n

solve1 :: (ImgAlgo, Img) -> Int
solve1 = solve' 2

solve2 :: (ImgAlgo, Img) -> Int
solve2 = solve' 50
