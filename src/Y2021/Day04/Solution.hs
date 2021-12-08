module Y2021.Day04.Solution where

import Debug.Trace
import Data.List
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day04/input.txt"
    putStrLn $ "2021.04.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.04.2: " ++ show (solve2 $ parseInput input)

data Cell = Cell Int Bool deriving Show

type Board = [[Cell]]

data BingoGame = Bingo [Int] [Board] deriving Show

parseBingoNums :: CharParser st [Int]
parseBingoNums = do
    nums <- sepBy (many digit) (char ',')
    newline
    return $ map read nums

parseBoards :: CharParser st [Board]
parseBoards = sepBy parseBoard newline
    where parseBoard = endBy1 parseBoardLine newline
          parseBoardLine = do
                    nums <- optional (char ' ') >> sepBy1 (many1 digit) (many1 (char ' '))
                    return $ map (\c -> Cell (read c) False) nums

parseBingoGame :: CharParser st BingoGame
parseBingoGame = do
    bingoNums <- parseBingoNums
    newline
    Bingo bingoNums <$> parseBoards

parseInput :: String -> BingoGame
parseInput i = case parse parseBingoGame "" i of
    Left e -> Bingo [] []
    Right bg -> bg

drawNum :: Int -> Board -> Board
drawNum n = map (drawNumRow n)

drawNumRow :: Int -> [Cell] -> [Cell]
drawNumRow n = map (updateCell n)

updateCell :: Int -> Cell -> Cell
updateCell n (Cell c True)  = Cell c True
updateCell n (Cell c False) = Cell c (n == c)

hasBingo :: Board -> Bool
hasBingo b = any rowHasBingo b || any rowHasBingo (transpose $ reverse b)
    where rowHasBingo = all cellMarked
          cellMarked (Cell _ True)  = True
          cellMarked (Cell _ False) = False

findFirstWinner :: BingoGame -> (Int, Board)
findFirstWinner (Bingo nums boards) = (lastNum, winningBoard)
    where (Just (lastNum, winningBoard), _) = foldl' applyNumAndCheckBingo (Nothing, boards) nums

applyNumAndCheckBingo :: (Maybe (Int, Board), [Board]) -> Int -> (Maybe (Int, Board), [Board])
applyNumAndCheckBingo (Just w, bs) _ = (Just w, bs)
applyNumAndCheckBingo (Nothing, updatedBoards) n =
    let nextBoards = map (drawNum n) updatedBoards in
    case filter hasBingo nextBoards of
        [] -> (Nothing, nextBoards)
        [x] -> (Just (n, x), nextBoards)
        _ -> error "Too many boards won"

findLastWinner :: BingoGame -> (Int, Board)
findLastWinner (Bingo nums boards) = (lastNum, winningBoard)
    where (Just (lastNum, winningBoard), _) = foldl' applyNumAndFilterBingos (Nothing, boards) nums

applyNumAndFilterBingos :: (Maybe (Int, Board), [Board]) -> Int -> (Maybe (Int, Board), [Board])
applyNumAndFilterBingos (Just w, bs) _ = (Just w, bs)
applyNumAndFilterBingos (Nothing, updatedBoards) n =
    let nextBoards = map (drawNum n) updatedBoards in
    case nextBoards of
        [] -> error "No boards won"
        [x] -> if hasBingo x then (Just (n, x), []) else (Nothing, filter (not . hasBingo) nextBoards)
        _ -> (Nothing, filter (not . hasBingo) nextBoards)

calculateScore :: (Int, Board) -> Int
calculateScore (n, b) = n * foldl' sumRow 0 b
    where sumRow s r = s + foldl' sumCell 0 r
          sumCell s (Cell c False) = s + c
          sumCell s (Cell _ True)  = s

solve1 :: BingoGame -> Int
solve1 = calculateScore . findFirstWinner

solve2 :: BingoGame -> Int
solve2 = calculateScore . findLastWinner
