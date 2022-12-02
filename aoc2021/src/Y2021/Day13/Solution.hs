module Y2021.Day13.Solution where

import Data.List
import qualified Data.Set as S
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day13/input.txt"
    putStrLn $ "2021.13.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.13.2: " ++ solve2 (parseInput input)

data Point = P Int Int deriving (Show, Ord, Eq)
data Fold = X Int | Y Int deriving (Show, Ord, Eq)

parsePoint :: CharParser st Point
parsePoint = do
    x <- many1 digit
    char ','
    y <- many1 digit
    return $ P (read x) (read y)

toFold :: (Char, Int) -> Fold
toFold ('y', y) = Y y
toFold ('x', x) = X x
toFold _ = error "unrecognized axis"

parseFold :: CharParser st Fold
parseFold = do
    string "fold along "
    axis <- char 'y' <|> char 'x'
    char '='
    n <- many1 digit
    return $ toFold (axis, read n)

parseLines :: CharParser st ([Point], [Fold])
parseLines = do
    points <- endBy parsePoint newline
    newline
    folds <- endBy parseFold newline
    return (points, folds)

parseInput :: String -> ([Point], [Fold])
parseInput i = case parse parseLines "" i of
    Left e -> ([], [])
    Right lines -> lines

fold :: S.Set Point -> Fold -> S.Set Point
fold ps (Y n) = S.foldr f S.empty ps
    where f p@(P x y) ps
            | y < n = S.insert p ps
            | y > n = S.insert (P x (2*n - y)) ps
            | otherwise = error "dots should never appear on a folded line"
fold ps (X n) = S.foldr f S.empty ps
    where f p@(P x y) ps
            | x < n = S.insert p ps
            | x > n = S.insert (P (2*n - x) y) ps
            | otherwise = error "dots should never appear on a folded line"

solve1 :: ([Point], [Fold]) -> Int
solve1 (ps, fs) = S.size $ fold (S.fromList ps) (head fs)

showPoints :: S.Set Point -> String
showPoints ps = showRow 0 []
    where maxX = S.foldr (\(P x _) m -> if x > m then x else m) 0 ps
          maxY = S.foldr (\(P _ y) m -> if y > m then y else m) 0 ps
          showCol x y s | x <= maxX = if S.member (P x y) ps then showCol (x+1) y ('#':s) else showCol (x+1) y ('.':s)
                        | otherwise = reverse s
          showRow y s | y <= maxY = showRow (y+1) (s ++ ('\n':showCol 0 y []))
                      | otherwise = s

solve2 :: ([Point], [Fold]) -> String
solve2 (ps, fs) = showPoints $ foldl' fold (S.fromList ps) fs
