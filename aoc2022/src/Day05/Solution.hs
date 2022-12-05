module Day05.Solution (solve) where

-- import Debug.Trace
import Data.Array ( Array, elems, listArray, (!), (//) )
import Data.List ( foldl', transpose )
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day05/input.txt"
    putStrLn $ "2022.05.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.05.2: " ++ show (solve2 $ parseInput input)

type Crate = Char
type Stack = [Crate]
type Instruction = (Int, Int, Int)

parseCrate :: CharParser st (Maybe Crate)
parseCrate = do
    _ <- char '['
    c <- letter
    _ <- char ']'
    return $ Just c

parseNone :: CharParser st (Maybe Crate)
parseNone = do
    _ <- char ' '
    _ <- char ' '
    _ <- char ' '
    return Nothing

parseRow :: CharParser st [Maybe Crate]
parseRow = sepBy1 (parseCrate <|> parseNone) (char ' ')

-- drops the maybe
toStack :: [Maybe Crate] -> Stack
toStack = foldr f []
    where f (Just c) cs = c:cs
          f Nothing  cs = cs

parseStacks :: CharParser st (Array Int Stack)
parseStacks = do
    rows <- endBy1 parseRow newline
    let stacks = map toStack $ transpose rows
    return $ listArray (1, length stacks) stacks

parseInstruction :: CharParser st Instruction
parseInstruction = do
    _ <- string "move "
    n <- many1 digit
    _ <- string " from "
    s1 <- many1 digit
    _ <- string " to "
    s2 <- many1 digit
    return (read n, read s1, read s2)

parseInstructions :: CharParser st [Instruction]
parseInstructions = endBy1 parseInstruction newline

parseRearrangements :: CharParser st (Array Int Stack, [Instruction])
parseRearrangements = do
    ss <- parseStacks
    _ <- newline
    is <- parseInstructions
    return (ss, is)

parseInput :: String -> (Array Int Stack, [Instruction])
parseInput i = case parse parseRearrangements "" i of
    Left e -> error (show e)
    Right es -> es

solve' :: ((Stack, Stack) -> Int -> (Stack, Stack)) -> (Array Int Stack, [Instruction]) -> String
solve' move (ss, is) = map head $ elems $ foldl' f ss is
    where f ss' (n, i1, i2) = ss' // [(i1, newS1), (i2, newS2)]
            where (newS1, newS2) = move (ss' ! i1, ss' ! i2) n

move1 :: (Stack, Stack) -> Int -> (Stack, Stack)
move1 (s1, s2) n = foldr move (s1, s2) [1..n]
    where move _ (x:xs, ys) = (xs, x:ys)
          move _ ([], _) = error ("can't move " ++ show n ++ " from: " ++ show s1 ++ " to " ++ show s2)

solve1 :: (Array Int Stack, [Instruction]) -> String
solve1 = solve' move1

move2 :: (Stack, Stack) -> Int -> (Stack, Stack)
move2 (s1, s2) n = (drop n s1, take n s1 ++ s2)

solve2 :: (Array Int Stack, [Instruction]) -> String
solve2 = solve' move2
