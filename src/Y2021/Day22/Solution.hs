module Y2021.Day22.Solution where

import Data.Array
import Data.List
import qualified Data.Set as S
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day22/input.txt"
    putStrLn $ "2021.22.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.22.2: " ++ show (solve2 $ parseInput input)

type Range = ((Int, Int, Int), (Int, Int, Int))
type Action = (Bool, Range)

parseState :: CharParser st Bool
parseState = do
    char 'o'
    s <- string "n" <|> string "ff"
    return $ s == "n"

parseNum :: CharParser st Int
parseNum = do
    n <- many1 (digit <|> char '-')
    return $ read n

parseRange :: CharParser st Range
parseRange = do
    string " x="
    x1 <- parseNum
    string ".."
    x2 <- parseNum
    string ",y="
    y1 <- parseNum
    string ".."
    y2 <- parseNum
    string ",z="
    z1 <- parseNum
    string ".."
    z2 <- parseNum
    return ((x1, y1, z1), (x2, y2, z2))

parseAction :: CharParser st Action
parseAction = do
    state <- parseState
    range <- parseRange
    return (state, range)

parseActions :: CharParser st [Action]
parseActions = endBy1 parseAction newline

parseInput :: String -> [Action]
parseInput i = case parse parseActions "" i of
    Left e -> error (show e)
    Right a -> a

inBounds1 :: Range -> Bool
inBounds1 ((a, b, c), (d, e, f)) = all (\x -> x <= 50 && x >= -50) [a, b, c, d, e, f]

inBounds2 :: Range -> Bool
inBounds2 = const True

solveNaive :: (Range -> Bool) -> [Action] -> Int
solveNaive bound as = S.size $ foldl' f S.empty as'
    where as' = filter (bound . snd) as
          f s (True,  r) = S.union      s $ S.fromList $ range r
          f s (False, r) = S.difference s $ S.fromList $ range r

overlap :: Range -> Range -> Range
overlap ((a1, b1, c1), (a2, b2, c2)) ((x1, y1, z1), (x2, y2, z2)) = ((max x1 a1, max y1 b1, max z1 c1), (min x2 a2, min y2 b2, min z2 c2))

eligible :: Range -> Bool
eligible ((a1, b1, c1), (a2, b2, c2)) = a2 >= a1 && b2 >= b1 && c2 >= c1

countOverlaps :: [Range] -> Int
countOverlaps = foldr c 0
    where c ((a1, b1, c1), (a2, b2, c2)) total = total + l
              where l = (c2 - c1 + 1) * (b2 - b1 + 1) * (a2 - a1 + 1)

solveEfficient :: (Range -> Bool) -> [Action] -> Int
solveEfficient bound as = countOverlaps toAdd - countOverlaps toSub
    where as' = filter (bound . snd) as
          (toAdd, toSub) = foldl' f ([], []) as'
          f (add, sub) a@(s, r) = (newToAdd ++ add, newToSub ++ sub)
              where newToSub = filter eligible $ map (overlap r) add
                    newToAdd' = filter eligible $ map (overlap r) sub
                    newToAdd = if s then r:newToAdd' else newToAdd'

solve1 :: [Action] -> Int
solve1 = solveEfficient inBounds1

solve2 :: [Action] -> Int
solve2 = solveEfficient inBounds2
