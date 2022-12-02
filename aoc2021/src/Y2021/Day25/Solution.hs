{-# LANGUAGE TupleSections #-}
module Y2021.Day25.Solution where

import Data.Array
import Data.List
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day25/input.txt"
    putStrLn $ "2021.25.1: " ++ show (solve1 $ parseInput input)
    -- putStrLn $ "2021.25.2: " ++ show (solve2 $ parseInput input)

data SeaCucumber = E | S | None deriving (Show, Ord, Eq)
type SeaFloor = Array (Int, Int) SeaCucumber

parseNone :: CharParser st SeaCucumber
parseNone = do
    char '.'
    return None

parseEast :: CharParser st SeaCucumber
parseEast = do
    char '>'
    return E

parseSouth :: CharParser st SeaCucumber
parseSouth = do
    char 'v'
    return S

parseLine :: CharParser st [SeaCucumber]
parseLine = many1 (parseNone <|> parseEast <|> parseSouth)

parseLines :: CharParser st SeaFloor
parseLines = do
    scs <- endBy parseLine newline
    return $ toFloor scs

parseInput :: String -> SeaFloor
parseInput i = case parse parseLines "" i of
    Left e -> error (show e)
    Right i -> i

toFloor :: [[SeaCucumber]] -> SeaFloor
toFloor l' = array bound associations
    where l = transpose l'
          r = length l
          c = length $ head l
          bound = ((0, 0), (r-1, c-1))
          cIndices = map (zip [0..c-1]) l
          rcIndices = zip [0..r-1] cIndices
          associations = concatMap associate rcIndices
          associate (r, cs) = map (\(c, i) -> ((r, c), i)) cs

getNeighbor :: SeaFloor -> (Int, Int) -> (Int, Int)
getNeighbor f i@(x, y)
    | f ! i == E = let i' = (x+1, y) in if inRange (bounds f) i' then i' else (0, y)
    | f ! i == S = let i' = (x, y+1) in if inRange (bounds f) i' then i' else (x, 0)
    | otherwise = error "no neighbor exists"

neighbor :: SeaFloor -> (Int, Int) -> SeaCucumber
neighbor f i = f ! getNeighbor f i

shouldMove :: SeaFloor -> (Int, Int) -> Bool
shouldMove f i | f ! i == None = False
               | otherwise = neighbor f i == None

showSC :: SeaCucumber -> Char
showSC E = '>'
showSC S = 'v'
showSC None = '.'

showFloor :: SeaFloor -> String
showFloor f = showRow 0 []
    where (_, (maxX, maxY)) = bounds f
          showCol x y s | x <= maxX = showCol (x+1) y (showSC (f ! (x, y)) : s)
                        | otherwise = reverse s
          showRow y s | y <= maxY = showRow (y+1) (s ++ ('\n':showCol 0 y []))
                      | otherwise = s

move :: SeaFloor -> SeaFloor
move f = array (bounds f) (M.assocs m')
    where r = range $ bounds f
          floorMap = M.fromList (map (, None) r)
          m = foldr moveE floorMap r
          moveE i fm | shouldMove f i =
                        case f ! i of
                        E -> M.insert (getNeighbor f i) E fm
                        S -> M.insert i S fm
                        None -> error "none shouldn't move"
                     | otherwise =
                        case f ! i of
                        None -> fm
                        _ -> M.insert i (f ! i) fm
          f' = array (bounds f) (M.assocs m)
          m' = foldr moveS floorMap r
          moveS i fm | shouldMove f' i =
                        case f' ! i of
                        E -> M.insert i E fm
                        S -> M.insert (getNeighbor f' i) S fm
                        None -> error "none shouldn't move"
                     | otherwise =
                        case f' ! i of
                        None -> fm
                        _ -> M.insert i (f' ! i) fm

solve1 :: SeaFloor -> Int
solve1 f = count f fs 1
    where fs = tail $ iterate move f
          count f [] _ = error "give me an infinite list bro"
          count f (f':fs) n
              | f == f' = n
              | otherwise = count f' fs (n + 1)
