module Day11.Solution (solve) where

-- import Debug.Trace
import Data.Array ( Array, elems, listArray, (!), (//), indices )
import Data.List ( sort, foldl' )
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day11/input.txt"
    -- test <- readFile "./src/Day11/test.txt"
    putStrLn $ "2022.11.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.11.2: " ++ show (solve2 $ parseInput input)

-- Monkey # inspections, items, op, mod, test
data Monkey = M Int [Int] (Int -> Int) Int (Int ->Int)

getOp :: Char -> String -> Int -> Int
getOp '*' "old" i = i * i
getOp '*' n i = i * read n
getOp '+' n i = i + read n
getOp op n _ = error ("unrecognized op " ++ [op] ++ " and n " ++ n)

getTest :: Int -> Int -> Int -> Int -> Int
getTest d t f i = if i `rem` d == 0 then t else f

parseMonkey :: CharParser st Monkey
parseMonkey = do
    _ <- string "Monkey "
    _ <- many1 digit
    _ <- char ':'
    _ <- newline

    _ <- string "  Starting items: "
    items <- sepBy1 (many1 digit) (string ", ")
    _ <- newline

    _ <- string "  Operation: new = old "
    op <- char '*' <|> char '+'
    _ <- char ' '
    opN <- many1 digit <|> string "old"
    _ <- newline

    _ <- string "  Test: divisible by "
    d <- many1 digit
    _ <- newline

    _ <- string "    If true: throw to monkey "
    t <- many1 digit
    _ <- newline

    _ <- string "    If false: throw to monkey "
    f <- many1 digit
    _ <- newline

    return $ M 0 (map read items) (getOp op opN) (read d) (getTest (read d) (read t) (read f))

parseMonkeys :: CharParser st (Array Int Monkey)
parseMonkeys = do
    ms <- sepBy1 parseMonkey newline
    return $ listArray (0, length ms - 1) ms

parseInput :: String -> Array Int Monkey
parseInput i = case parse parseMonkeys "" i of
    Left e -> error (show e)
    Right ms -> ms

-- assumes a!i has more than one item
toss :: Array Int Monkey -> Int -> Int -> Int -> Array Int Monkey
toss a i j n = a // [(i, M (iInsps + 1) (tail iItems) iop idiv itest), (j, M jInsps (jItems ++ [n]) jop jdiv jtest)]
    where M iInsps iItems iop idiv itest = a ! i
          M jInsps jItems jop jdiv jtest = a ! j

monkeyRound :: (Int -> Int) -> Array Int Monkey -> Array Int Monkey
monkeyRound worry a = foldl' inspectAll a (indices a)
    where inspectAll a' m = foldl' inspect a' is
            where M _ is op _ test = a' ! m
                  inspect a'' i = let n = worry $ op i in toss a'' m (test n) n

monkeyRounds :: (Int -> Int) -> Int -> Array Int Monkey -> Array Int Monkey
monkeyRounds _ 0 a = a
monkeyRounds worry n a = monkeyRounds worry (n - 1) (monkeyRound worry a)

solve' :: (Int -> Int) -> Int -> Array Int Monkey -> Int
solve' worry r ms = product . take 2 . reverse . sort $ map (\(M is _ _ _ _) -> is) (elems $ monkeyRounds worry r ms)

solve1 :: Array Int Monkey -> Int
solve1 = solve' (`div` 3) 20

solve2 :: Array Int Monkey -> Int
solve2 ms = solve' (`mod` modulo) 10000 ms
    where modulo = product $ map (\(M _ _ _ mdiv _) -> mdiv) (elems ms)
