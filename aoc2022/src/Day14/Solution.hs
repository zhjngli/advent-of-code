module Day14.Solution (solve) where

-- import Debug.Trace
import Data.Array
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day14/input.txt"
    putStrLn $ "2022.14.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.14.2: " ++ show (solve2 $ parseInput input)

-- ended up not needing this type and could've just used a set instead of a map but too lazy now
data Cave = Rock | Sand

parseRockLines :: CharParser st [[(Int, Int)]]
parseRockLines = endBy1 parseRockLine newline
    where parseRockLine = sepBy1 parseRock (string " -> ")
          parseRock = do
            x <- many1 digit
            _ <- char ','
            y <- many1 digit
            return (read x, read y)

parseInput :: String -> [[(Int, Int)]]
parseInput i = case parse parseRockLines "" i of
    Left e -> error (show e)
    Right ls -> ls

getRange :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
getRange (x1, y1) (x2, y2)
    | x1 == x2 && y1 <= y2 = range ((x1, y1), (x2, y2))
    | x1 <= x2 && y1 == y2 = range ((x1, y1), (x2, y2))
    | x1 == x2 && y1 >  y2 = range ((x2, y2), (x1, y1))
    | x1 >  x2 && y1 == y2 = range ((x2, y2), (x1, y1))
    | otherwise = error ("rock line is not a straight line: " ++ show (x1, y1) ++ ", " ++ show (x2, y2))


rockLineToRange :: [(Int, Int)] -> [(Int, Int)]
rockLineToRange [] = []
rockLineToRange [r] = [r]
rockLineToRange (r1:r2:rs) = getRange r1 r2 ++ rockLineToRange (r2:rs)

rockLinesToCaveMap :: [[(Int, Int)]] -> M.Map (Int, Int) Cave
rockLinesToCaveMap ls = foldr f M.empty $ concatMap rockLineToRange ls
    where f i m = M.insert i Rock m

lowestRock :: M.Map (Int, Int) Cave -> Int
lowestRock m = foldr f 0 $ M.keys m
    where f (_, y) l = if y > l then y else l

dropSand1 :: (Int, Int) -> M.Map (Int, Int) Cave -> Int -> Maybe (Int, Int)
dropSand1 (x, y) m low
    | y > low = Nothing
    | M.notMember (  x, y+1) m = dropSand1 (  x, y+1) m low
    | M.notMember (x-1, y+1) m = dropSand1 (x-1, y+1) m low
    | M.notMember (x+1, y+1) m = dropSand1 (x+1, y+1) m low
    | otherwise = Just (x, y)

countSand1 :: Int -> M.Map (Int, Int) Cave -> Int -> Int
countSand1 low m n = case dropSand1 (500, 0) m low of
    Nothing -> n
    Just p -> countSand1 low (M.insert p Sand m) (n+1) 

solve1 :: [[(Int, Int)]] -> Int
solve1 ls = countSand1 low m 0
    where m = rockLinesToCaveMap ls
          low = lowestRock m

dropSand2 :: (Int, Int) -> M.Map (Int, Int) Cave -> Int -> (Int, Int)
dropSand2 (x, y) m caveFloor
    | y == caveFloor - 1 = (x, y)
    | M.notMember (  x, y+1) m = dropSand2 (  x, y+1) m caveFloor
    | M.notMember (x-1, y+1) m = dropSand2 (x-1, y+1) m caveFloor
    | M.notMember (x+1, y+1) m = dropSand2 (x+1, y+1) m caveFloor
    | otherwise = (x, y)

countSand2 :: Int -> M.Map (Int, Int) Cave -> Int -> Int
countSand2 caveFloor m n = case dropSand2 (500, 0) m caveFloor of
    (500, 0) -> n + 1
    p -> countSand2 caveFloor (M.insert p Sand m) (n+1) 

solve2 :: [[(Int, Int)]] -> Int
solve2 ls = countSand2 caveFloor m 0
    where m = rockLinesToCaveMap ls
          caveFloor = lowestRock m + 2
