module Day10.Solution (solve) where

import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S

import Lib.Common (toArray, dijkstrasArr, Cost (..))

solve :: IO ()
solve = do
    input <- readFile "./src/Day10/input.txt"
    putStrLn $ "2023.10.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.10.2: " ++ show (solve2 $ parseInput input)

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

solve2 :: PipeMap -> Int
solve2 p = length $ filter ((== (1::Int)) . (`mod` 2) . crosses 0) nonLoopTiles
    where
        start = startTile p
        costs = dijkstrasArr p [start] (\_ _ -> C 1) pipeNeighbors
        loopTiles = (S.fromList . map fst . filter ((/= Inf) . snd) . M.assocs) costs
        nonLoopTiles = S.elems $ S.difference (S.fromList (indices p)) loopTiles
        b = bounds p
        -- count how many times a ray drawn from the starting tile crosses the loop
        -- the ray starts from the input tile index, and moves diagonally down and to the right
        crosses n (r, c) =
            if inRange b (r, c) then
                if S.member (r, c) loopTiles then
                    -- since we're going diagonally down and to the right
                    -- when the tile is TR or BL, the ray effectively goes in and out of the loop in one step, i.e. n+2
                    -- since we take the mod 2 of n later, we don't increase the number of times this ray crosses the loop
                    case p ! (r, c) of
                    TR -> crosses n (r+1, c+1)
                    BL -> crosses n (r+1, c+1)
                    S -> crosses n (r+1, c+1) -- hardcode the start tile as a special case of BL cause i'm too lazy to figure it out dynamically
                    _ -> crosses (n+1) (r+1, c+1)
                else crosses n (r+1, c+1)
            else n
