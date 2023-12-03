{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Day02.Solution (solve) where

import Text.ParserCombinators.Parsec
import Data.List (foldl')

solve :: IO ()
solve = do
    input <- readFile "./src/Day02/input.txt"
    putStrLn $ "2023.02.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.02.2: " ++ show (solve2 $ parseInput input)

data Cubes = R Int | G Int | B Int deriving Show
type Game = (Int, [[Cubes]])

parseGame :: CharParser st Game
parseGame = do
    _ <- string "Game "
    gid <- many1 digit
    _ <- string ": "
    cs <- sepBy parseCubes (string "; ")
    return (read gid, cs)
    where
        parseCube = do
            n' <- many1 digit
            _ <- char ' '
            c <- choice [string "red", string "blue", string "green"]
            return (
                let n = read n' in
                case c of
                    "red" -> R n
                    "blue" -> B n
                    "green" -> G n
                    _ -> error "unrecognized color cube"
                )
        parseCubes = sepBy1 parseCube (string ", ")

parseGames :: CharParser st [Game]
parseGames = endBy1 parseGame newline

parseInput :: String -> [Game]
parseInput i = case parse parseGames "" i of
    Left e -> error (show e)
    Right gs -> gs

countCubes :: [Cubes] -> (Cubes, Cubes, Cubes)
countCubes = foldl'
        (\(R r, G g, B b) c ->
            case c of
            R r' -> (R (r+r'), G g, B b)
            G g' -> (R r, G (g+g'), B b)
            B b' -> (R r, G g, B (b+b'))
        )
        (R 0, G 0, B 0)

possibleGame :: Game -> Bool
possibleGame (_, css) = all possibleHandful css
    where
        possibleHandful cs = let (R r, G g, B b) = countCubes cs in
            r <= 12 && g <= 13 && b <= 14

solve1 :: [Game] -> Int
solve1 = sum . map fst . filter possibleGame

gamePower :: Game -> Int
gamePower (_, css) =
    let mincs = map countCubes css
        (R r, G g, B b) =
            foldl'
                (\(R r1, G g1, B b1) (R r2, G g2, B b2) -> (R (max r1 r2), G (max g1 g2), B (max b1 b2)))
                (head mincs)
                (tail mincs)
    in r * g * b

solve2 :: [Game] -> Int
solve2 = sum . map gamePower
