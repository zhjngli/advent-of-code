module Y2021.Day24.Solution where

import Data.List
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day24/input.txt"
    putStrLn $ "2021.24.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.24.2: " ++ show (solve2 $ parseInput input)

parseBlock :: CharParser st (Int, Int, Int)
parseBlock = do
    string "inp w"
    newline
    string "mul x 0"
    newline
    string "add x z"
    newline
    string "mod x 26"
    newline
    string "div z "
    zdiv <- many1 digit
    newline
    string "add x "
    xadd <- many1 (digit <|> char '-')
    newline
    string "eql x w"
    newline
    string "eql x 0"
    newline
    string "mul y 0"
    newline
    string "add y 25"
    newline
    string "mul y x"
    newline
    string "add y 1"
    newline
    string "mul z y"
    newline
    string "mul y 0"
    newline
    string "add y w"
    newline
    string "add y "
    yadd <- many1 digit
    newline
    string "mul y x"
    newline
    string "add z y"
    return (read xadd, read yadd, read zdiv)

parseBlockInputs :: CharParser st [(Int, Int, Int)]
parseBlockInputs = endBy1 parseBlock newline

parseInput :: String -> [(Int, Int, Int)]
parseInput i = case parse parseBlockInputs "" i of
    Left e -> error (show e)
    Right a -> a

-- W, X, Y, Z
type Env = (Int, Int, Int, Int)

-- Z_i = n_i + 26 * (n_i-1 + 26 * (n_i-2 + ... 26 * n_0))

functionBlock :: Int -> Int -> Int -> Int -> Env -> Env
functionBlock win xadd yadd zdiv =
    (\(w, x, y, z) -> (w, x, y, z + y)) .
    (\(w, x, y, z) -> (w, x, y * x, z)) .
    (\(w, x, y, z) -> (w, x, y + yadd, z)) .
    (\(w, x, y, z) -> (w, x, y + w, z)) .
    (\(w, x, y, z) -> (w, x, y * 0, z)) .
    (\(w, x, y, z) -> (w, x, y, z * y)) .
    (\(w, x, y, z) -> (w, x, y + 1, z)) .
    (\(w, x, y, z) -> (w, x, y * x, z)) .
    (\(w, x, y, z) -> (w, x, y + 25, z)) .
    (\(w, x, y, z) -> (w, x, y * 0, z)) .
    (\(w, x, y, z) -> (w, if x == 0 then 1 else 0, y, z)) .
    (\(w, x, y, z) -> (w, if x == w then 1 else 0, y, z)) .
    (\(w, x, y, z) -> (w, x + xadd, y, z)) .
    (\(w, x, y, z) -> (w, x, y, z `div` zdiv)) .
    (\(w, x, y, z) -> (w, x `mod` 26, y, z)) .
    (\(w, x, y, z) -> (w, x + z, y, z)) .
    (\(w, x, y, z) -> (w, x * 0, y, z)) .
    (\(w, x, y, z) -> (win, x, y, z))

modelNumberDigits :: [Int]
modelNumberDigits = [9, 8, 7, 6, 5, 4, 3, 2, 1]

modelNumbers :: [[Int]]
modelNumbers =
    [[a, b, c, d, e, f, g, h, i, j, k, l, m, n]
    | a <- modelNumberDigits
    , b <- modelNumberDigits
    , c <- modelNumberDigits
    , d <- modelNumberDigits
    , e <- modelNumberDigits
    , f <- modelNumberDigits
    , g <- modelNumberDigits
    , h <- modelNumberDigits
    , i <- modelNumberDigits
    , j <- modelNumberDigits
    , k <- modelNumberDigits
    , l <- modelNumberDigits
    , m <- modelNumberDigits
    , n <- modelNumberDigits
    ]

verify :: [Int] -> [(Int, Int, Int)] -> Env
verify ds vs = out
    where ins = zip ds vs
          out = foldl' f (0, 0, 0, 0) ins
          f e (w, (x, y, z)) = functionBlock w x y z e

modifyAt :: Int -> [Int] -> Int -> [Int]
modifyAt i input prev = let (xs, _:ys) = splitAt i input in xs ++ prev:ys

fixModelNumber :: [(Int, Int, Int)] -> [(Int, Int)] -> [Int] -> Int -> [Int]
fixModelNumber [] _ input _ = input
fixModelNumber ((xadd, yadd,  1):vs) stack input i =
    fixModelNumber vs ((i, yadd):stack) input (i+1)
fixModelNumber ((xadd, yadd, 26):vs) ((j, prevAdd):stack) input i =
    fixModelNumber vs stack input'' (i+1)
    where prevInp = input !! j
          newInpUnbounded = prevInp + prevAdd + xadd
          (newInpUpperBound, prevInpAdjust) = if newInpUnbounded > 9 then (9, prevInp - (newInpUnbounded - 9)) else (newInpUnbounded, prevInp)
          (newInpBounded, prevInpFinal) = if newInpUpperBound < 1 then (1, prevInpAdjust + 1 - newInpUpperBound) else (newInpUpperBound, prevInpAdjust)
          input' = modifyAt i input newInpBounded
          input'' = modifyAt j input' prevInpFinal
fixModelNumber vs s i id = error ("unrecognized:\nvs: " ++ show vs ++ "\nstack: " ++ show s ++ "\ninput: " ++ show i)

solve1 :: [(Int, Int, Int)] -> Int
solve1 vs = (read . concatMap show) modelNumber
    where modelNumber = fixModelNumber vs [] (replicate 14 9) 0

solve2 :: [(Int, Int, Int)] -> Int
solve2 vs = (read . concatMap show) modelNumber
    where modelNumber = fixModelNumber vs [] (replicate 14 1) 0
