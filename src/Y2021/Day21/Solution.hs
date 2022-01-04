module Y2021.Day21.Solution where

import Debug.Trace
import Data.List
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day21/input.txt"
    putStrLn $ "2021.21.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.21.2: " ++ show (solve2 $ parseInput input)

data Game = Game { pos1 :: Int,
                   score1 :: Int,
                   pos2 :: Int,
                   score2 :: Int,
                   diceVal :: Int,
                   diceRolls :: Int,
                   p1Turn :: Bool } deriving (Show, Ord, Eq)

parseInitGame :: CharParser st Game
parseInitGame = do
    string "Player 1 starting position: "
    pos1 <- read <$> many1 digit
    newline
    string "Player 2 starting position: "
    pos2 <- read <$> many1 digit
    newline
    return $ Game pos1 0 pos2 0 1 0 True

parseInput :: String -> Game
parseInput i = case parse parseInitGame "" i of
    Left e -> Game 0 0 0 0 0 0 True
    Right g -> g

getPos :: Int -> Int -> Int
getPos i 0 = i
getPos i m = if i + 1 > 10 then getPos 1 (m-1) else getPos (i+1) (m-1)

nextDetDiceVal :: Int -> Int
nextDetDiceVal d = if d + 1 > 100 then 1 else d + 1

detTurn :: Game -> Game
detTurn (Game p1 s1 p2 s2 dv dr True)  = Game p1' (s1 + p1') p2 s2 dv' (dr + 3) False
    where d1 = nextDetDiceVal dv
          d2 = nextDetDiceVal d1
          dv' = nextDetDiceVal d2
          m = dv + d1 + d2
          p1' = getPos p1 m
detTurn (Game p1 s1 p2 s2 dv dr False) = Game p1 s1 p2' (s2 + p2') dv' (dr + 3) True
    where d1 = nextDetDiceVal dv
          d2 = nextDetDiceVal d1
          dv' = nextDetDiceVal d2
          m = dv + d1 + d2
          p2' = getPos p2 m

detGameEnd :: [Game] -> Int
detGameEnd [] = error "give me an infinite list bro"
detGameEnd ((Game p1 s1 p2 s2 dv dr _):gs)
    | s1 >= 1000 = s2 * dr
    | s2 >= 1000 = s1 * dr
    | otherwise = detGameEnd gs

solve1 :: Game -> Int
solve1 g = detGameEnd $ iterate detTurn g

diracDicePossibilities :: [(Int, Int)]
diracDicePossibilities = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

-- no memoization is too slow
diracWins :: Int -> Int -> [(Game, Int)] -> Int
diracWins w1 w2 [] = max w1 w2
diracWins w1 w2 gs = diracWins w1' w2' gs'
    where (w1', w2', gs') = foldr countWins (w1, w2, []) gs''
          countWins gn@(Game _ s1 _ s2 _ _ _, n) (win1, win2, l)
              | s1 >= 21  = (win1 + n, win2, l)
              | s2 >= 21  = (win1, win2 + n, l)
              | otherwise = (win1, win2, gn:l)
          gs'' = concatMap splitGame gs
          splitGame gn = map (newPossibilities gn) diracDicePossibilities
          newPossibilities (Game p1 s1 p2 s2 dv dr True,  n) (m, p) = (Game p1' s1' p2 s2 dv dr False, n * p)
              where p1' = getPos p1 m
                    s1' = s1 + p1'
          newPossibilities (Game p1 s1 p2 s2 dv dr False, n) (m, p) = (Game p1 s1 p2' s2' dv dr True, n * p)
              where p2' = getPos p2 m
                    s2' = s2 + p2'

diracWinsMemo :: Game -> M.Map Game (Int, Int) -> ((Int, Int), M.Map Game (Int, Int))
diracWinsMemo g@(Game p1 s1 p2 s2 dv dr o) memo
    | M.member g memo = (memo M.! g, memo)
    | s1 >= 21 = let r = (1, 0) in (r, M.insert g r memo)
    | s2 >= 21 = let r = (0, 1) in (r, M.insert g r memo)
    | otherwise = (r, M.insert g r memo')
    where (r, memo') = foldr f ((0, 0), memo) newGames
          f (g, n) ((a, b), memo) = let ((ga, gb), gmemo) = diracWinsMemo g memo in ((ga*n + a, gb*n + b), gmemo)
          newGames = concatMap splitGame [g]
          splitGame gn = map (newPossibilities gn) diracDicePossibilities
          newPossibilities (Game p1 s1 p2 s2 dv dr True) (m, p) = (Game p1' s1' p2 s2 dv dr False, p)
              where p1' = getPos p1 m
                    s1' = s1 + p1'
          newPossibilities (Game p1 s1 p2 s2 dv dr False) (m, p) = (Game p1 s1 p2' s2' dv dr True, p)
              where p2' = getPos p2 m
                    s2' = s2 + p2'

solve2 :: Game -> Int
solve2 g | a > b     = a
         | otherwise = b
         where (a, b) = fst $ diracWinsMemo g M.empty
-- solve2 g = diracWins 0 0 [(g, 1)]
