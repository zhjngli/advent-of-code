module Day21.Solution (solve) where

-- import Debug.Trace
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day21/input.txt"
    putStrLn $ "2022.21.1: " ++ show (solve1 $ parseInput input)
    -- putStrLn $ "2022.21.2: " ++ show (solve2 $ parseInput input)

data Op = Add | Mul | Sub | Div deriving Show
data Monkey = M Op (String, String) | I Integer deriving Show

parseLeaf :: CharParser st Monkey
parseLeaf = do
    i <- many1 digit
    return $ I (read i)

toOp :: Char -> Op
toOp '+' = Add
toOp '*' = Mul
toOp '-' = Sub
toOp '/' = Div
toOp c   = error ("unrecognized char " ++ [c])

parseOp :: CharParser st Monkey
parseOp = do
    x <- many1 letter
    _ <- char ' '
    op <- oneOf "+*-/"
    _ <- char ' '
    y <- many1 letter
    return $ M (toOp op) (x, y)

parseMonkeyLine :: CharParser st (String, Monkey)
parseMonkeyLine = do
    name <- many1 letter
    _ <- string ": "
    monkey <- parseLeaf <|> parseOp
    return (name, monkey)

parseMonkeys :: CharParser st [(String, Monkey)]
parseMonkeys = endBy1 parseMonkeyLine newline

monkeyMap :: [(String, Monkey)] -> M.Map String Monkey
monkeyMap = foldr f M.empty
    where f (n, m) ms = M.insert n m ms

parseInput :: String -> [(String, Monkey)]
parseInput i = case parse parseMonkeys "" i of
    Left e -> error (show e)
    Right ms -> ms

mOp :: Op -> Integer -> Integer -> Integer
mOp Add x y = x + y
mOp Mul x y = x * y
mOp Sub x y = x - y
mOp Div x y = x `div` y

getMonkeyInt :: M.Map String Monkey -> String -> Integer
getMonkeyInt mm s = case M.findWithDefault (I 0) s mm of
    I i -> i
    M o (x, y) -> error ("monkey " ++ s ++ " should not return " ++ show o ++ "(" ++ x ++ ", " ++ y ++ ")")

calculateMonkey :: [(String, Monkey)] -> M.Map String Monkey -> M.Map String Monkey
calculateMonkey [] mm = mm
calculateMonkey ((n, I i):ms) mm = calculateMonkey ms (M.insert n (I i) mm)
calculateMonkey ((n, M o (x, y)):ms) mm =
    if M.member x mm && M.member y mm then calculateMonkey ms (M.insert n (I $ mOp o (getMonkeyInt mm x) (getMonkeyInt mm y)) mm)
    else calculateMonkey (ms ++ [(n, M o (x, y))]) mm

solve1 :: [(String, Monkey)] -> Maybe Monkey
solve1 ms = M.lookup "root" $ calculateMonkey ms M.empty
