module Day02.Solution where

import Data.List
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day02/input.txt"
    putStrLn $ "2022.02.1: " ++ show (solve1 $ parseInput1 input)
    putStrLn $ "2022.02.2: " ++ show (solve2 $ parseInput2 input)

data Hand = R | P | S
data Res = W | L | D

readHand :: Char -> Hand
readHand 'A' = R
readHand 'B' = P
readHand 'C' = S
readHand 'X' = R
readHand 'Y' = P
readHand 'Z' = S
readHand _ = error "unknown hand"

parseRound1 :: CharParser st (Hand, Hand)
parseRound1 = do
    one <- char 'A' <|> char 'B' <|> char 'C'
    _ <- char ' '
    two <- char 'X' <|> char 'Y' <|> char 'Z'
    return (readHand one, readHand two)

parseRounds1 :: CharParser st [(Hand, Hand)]
parseRounds1 = endBy1 parseRound1 newline

parseInput1 :: String -> [(Hand, Hand)]
parseInput1 i = case parse parseRounds1 "" i of
    Left e -> error (show e)
    Right hs -> hs

readRes :: Char -> Res
readRes 'X' = L
readRes 'Y' = D
readRes 'Z' = W
readRes _ = error "unknown result"

parseRound2 :: CharParser st (Hand, Res)
parseRound2 = do
    h <- char 'A' <|> char 'B' <|> char 'C'
    _ <- char ' '
    r <- char 'X' <|> char 'Y' <|> char 'Z'
    return (readHand h, readRes r)

parseRounds2 :: CharParser st [(Hand, Res)]
parseRounds2 = endBy1 parseRound2 newline

parseInput2 :: String -> [(Hand, Res)]
parseInput2 i = case parse parseRounds2 "" i of
    Left e -> error (show e)
    Right hs -> hs

scoreHand :: Hand -> Int
scoreHand R = 1
scoreHand P = 2
scoreHand S = 3

scoreRound1 :: (Hand, Hand) -> Int
scoreRound1 (h1, h2) = compareHand h2 h1 + scoreHand h2
    where compareHand R R = 3
          compareHand R P = 0
          compareHand R S = 6
          compareHand P R = 6
          compareHand P P = 3
          compareHand P S = 0
          compareHand S R = 0
          compareHand S P = 6
          compareHand S S = 3

solve1 :: [(Hand, Hand)] -> Int
solve1 rounds = sum $ map scoreRound1 rounds

scoreRes :: Res -> Int
scoreRes W = 6
scoreRes D = 3
scoreRes L = 0

scoreRound2 :: (Hand, Res) -> Int
scoreRound2 (h, r) = scoreRes r + scoreHand (findHand h r)
    where findHand R W = P
          findHand R D = R
          findHand R L = S
          findHand P W = S
          findHand P D = P
          findHand P L = R
          findHand S W = R
          findHand S D = S
          findHand S L = P

solve2 :: [(Hand, Res)] -> Int
solve2 rounds = sum $ map scoreRound2 rounds
