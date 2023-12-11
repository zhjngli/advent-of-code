module Day10.Solution (solve) where

import Data.Array
import Data.Map as M (elems)
import Lib.Common (toArray, dijkstrasArr, Cost (..))

solve :: IO ()
solve = do
    input <- readFile "./src/Day10/input.txt"
    putStrLn $ "2023.10.1: " ++ show (solve1 $ parseInput input)
    -- putStrLn $ "2023.10.2: " ++ show (solve2 $ parseInput input)

data Tile = H | V | TL | TR | BL | BR | S | E deriving (Show, Eq)
type PipeMap = Array (Int, Int) Tile

charToTile :: Char -> Tile
charToTile 'S' = S
charToTile '-' = H
charToTile '|' = V
charToTile 'J' = TL
charToTile 'L' = TR
charToTile '7' = BL
charToTile 'F' = BR
charToTile '.' = E
charToTile c = error $ "unknown tile: " ++ [c]

startTile :: PipeMap -> (Int, Int)
startTile = fst . head . filter ((== S) . snd) . assocs

pipeNeighbors :: PipeMap -> (Int, Int) -> [(Int, Int)]
pipeNeighbors p i@(r,c) = case p ! i of
    H -> [(r, c-1), (r, c+1)]
    V -> [(r-1, c), (r+1, c)]
    TL -> [(r-1, c), (r, c-1)]
    TR -> [(r-1, c), (r, c+1)]
    BL -> [(r+1, c), (r, c-1)]
    BR -> [(r+1, c), (r, c+1)]
    E -> []
    S -> filter (elem i . pipeNeighbors p) [(r, c-1), (r, c+1), (r-1, c), (r+1, c)]

parseInput :: String -> PipeMap
parseInput i = toArray (map (map charToTile) (lines i))

solve1 :: PipeMap -> Cost Int
solve1 p = maximum . filter (/= Inf) $ M.elems costs
    where
        start = startTile p
        costs = dijkstrasArr p [start] (\_ _ -> C 1) pipeNeighbors
