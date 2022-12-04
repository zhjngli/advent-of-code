module Day03.Solution (solve) where

import Data.List as L
import Data.Set as S
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day03/input.txt"
    putStrLn $ "2022.03.1: " ++ show (solve1 $ parseInput1 input)
    putStrLn $ "2022.03.2: " ++ show (solve2 $ parseInput2 input)

type SplitBag = (Set Char, Set Char)
type Bag = Set Char
type ElfGroup = (Bag, Bag, Bag)

parseSplitBags :: CharParser st [SplitBag]
parseSplitBags = do
    bags <- endBy1 (many1 letter) newline
    return $ L.map splitBag bags
        where splitBag b = let (l, r) = L.splitAt (length b `div` 2) b in (S.fromList l, S.fromList r)

parseInput1 :: String -> [SplitBag]
parseInput1 i = case parse parseSplitBags "" i of
    Left e -> error (show e)
    Right bs -> bs

parseElfGroup :: CharParser st ElfGroup
parseElfGroup = do
    bag1 <- many1 letter
    _ <- newline
    bag2 <- many1 letter
    _ <- newline
    bag3 <- many1 letter
    return (S.fromList bag1, S.fromList bag2, S.fromList bag3)

parseElfGroups :: CharParser st [ElfGroup]
parseElfGroups = endBy1 parseElfGroup newline

parseInput2 :: String -> [ElfGroup]
parseInput2 i = case parse parseElfGroups "" i of
    Left e -> error (show e)
    Right es -> es

priorities :: [Char]
priorities = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

getItemPriority :: Char -> Int
getItemPriority i =
    case L.elemIndex i priorities of
    Just n -> n + 1
    Nothing -> error ("cannot find " ++ show i ++ " in priorities")

solve1 :: [SplitBag] -> Int
solve1 bs = sum $ L.map f bs
    where f (l, r) =
            let s = S.elems $ S.intersection l r in
                case L.length s of
                1 -> getItemPriority (head s)
                _ -> error ("too many common items in (" ++ show l ++ ", " ++ show r ++ ")")

solve2 :: [ElfGroup] -> Int
solve2 es = sum $ L.map f es
    where f (e1, e2, e3) =
            let s = S.elems $ S.intersection e1 (S.intersection e2 e3) in
                case L.length s of
                1 -> getItemPriority (head s)
                _ -> error ("too many common items in (" ++ show e1 ++ ", " ++ show e2 ++ ", " ++ show e3 ++ ")")
