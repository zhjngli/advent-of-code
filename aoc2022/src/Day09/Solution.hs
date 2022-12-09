module Day09.Solution (solve) where

-- import Debug.Trace
import Data.List ( foldl', group, sort )
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day09/input.txt"
    putStrLn $ "2022.09.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.09.2: " ++ show (solve2 $ parseInput input)

charToMovement :: Char -> (Int, Int)
charToMovement 'L' = (-1, 0)
charToMovement 'R' = (1, 0)
charToMovement 'U' = (0, 1)
charToMovement 'D' = (0, -1)
charToMovement c = error ("unknown movement: " ++ [c])

parseHeadMovement :: CharParser st [(Int, Int)]
parseHeadMovement = do
    dir <- char 'L' <|> char 'R' <|> char 'U' <|> char 'D'
    _ <- char ' '
    n <- many1 digit
    return $ replicate (read n) (charToMovement dir)

parseHeadMovements :: CharParser st [[(Int, Int)]]
parseHeadMovements = endBy1 parseHeadMovement newline

parseInput :: String -> [(Int, Int)]
parseInput i = case parse parseHeadMovements "" i of
    Left e -> error (show e)
    Right ms -> concat ms

touching :: (Int, Int) -> (Int, Int) -> Bool
touching (hx, hy) (tx, ty) = abs (hx - tx) <= 1 && abs (hy - ty) <= 1

tailMove :: (Int, Int) -> (Int, Int) -> (Int, Int)
tailMove (hx, hy) (tx, ty)
    -- head 2 units right
    | hx - tx == 2 && hy == ty = (tx+1, ty)
    -- head 2 units left
    | hx - tx == -2 && hy == ty = (tx-1, ty)
    -- head 2 units up
    | hx == tx && hy - ty == 2 = (tx, ty+1)
    -- head 2 units down
    | hx == tx && hy - ty == -2 = (tx, ty-1)
    -- head 2 units right 1 unit up
    | hx - tx == 2 && hy - ty == 1 = (tx+1, ty+1)
    -- head 1 unit right 2 units up
    | hx - tx == 1 && hy - ty == 2 = (tx+1, ty+1)
    -- head 1 unit left 2 units up
    | hx - tx == -1 && hy - ty == 2 = (tx-1, ty+1)
    -- head 2 units left 1 unit up
    | hx - tx == -2 && hy - ty == 1 = (tx-1, ty+1)
    -- head 2 units left 1 unit down
    | hx - tx == -2 && hy - ty == -1 = (tx-1, ty-1)
    -- head 1 unit left 2 units down
    | hx - tx == -1 && hy - ty == -2 = (tx-1, ty-1)
    -- head 1 unit right 2 units down
    | hx - tx == 1 && hy - ty == -2 = (tx+1, ty-1)
    -- head 2 units right 1 unit down
    | hx - tx == 2 && hy - ty == -1 = (tx+1, ty-1)
    | touching (hx, hy) (tx, ty) = (tx, ty)
    -- head 2 up right
    | hx - tx == 2 && hy - ty == 2 = (tx+1, ty+1)
    -- head 2 up left
    | hx - tx == -2 && hy - ty == 2 = (tx-1, ty+1)
    -- head 2 down left
    | hx - tx == -2 && hy - ty == -2 = (tx-1, ty-1)
    -- head 2 down right
    | hx - tx == 2 && hy - ty == -2 = (tx+1, ty-1)
tailMove h t = error ("head " ++ show h ++ " is too far from tail " ++ show t)

move :: (Int, Int) -> (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
move (hx, hy) (tx, ty) (mx, my) = ((hx', hy'), (tx', ty'))
    where (hx', hy') = (hx + mx, hy + my)
          (tx', ty') = tailMove (hx', hy') (tx, ty)

countUniq :: Ord a => [a] -> Int
countUniq = length . group . sort

solve1 :: [(Int, Int)] -> Int
solve1 ms = countUniq . snd $ foldl' f ((0, 0), [(0, 0)]) ms
    where f (h, t:ts) m = let (h', t') = move h t m in (h', t':t:ts)
          f (_, []) _ = error "should never happen"

solve2 :: [(Int, Int)] -> Int
solve2 ms = countUniq . ninth $ foldl' f ((0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), [(0, 0)]) ms
    where f (h, t1, t2, t3, t4, t5, t6, t7, t8, t9:t9s) m =
            let (h', t1') = move h t1 m
                t2' = tailMove t1' t2
                t3' = tailMove t2' t3
                t4' = tailMove t3' t4
                t5' = tailMove t4' t5
                t6' = tailMove t5' t6
                t7' = tailMove t6' t7
                t8' = tailMove t7' t8
                t9' = tailMove t8' t9
            in (h', t1', t2', t3', t4', t5', t6', t7', t8', t9':t9:t9s)
          f (_, _, _, _, _, _, _, _, _, []) _ = error "should never happen"
          ninth (_, _, _, _, _, _, _, _, _, t9s) = t9s
