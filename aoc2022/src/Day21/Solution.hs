module Day21.Solution (solve) where

-- import Debug.Trace
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day21/input.txt"
    putStrLn $ "2022.21.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.21.2: " ++ show (solve2 $ parseInput input)

data Op = Add | Mul | Sub | Div deriving Show
data Monkey = M Op (String, String) | I Integer deriving Show
data MonkeyTree = Root MonkeyTree MonkeyTree | Node Op MonkeyTree MonkeyTree | Leaf Integer | Var deriving Show

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

getMonkeyTreeNode :: M.Map String MonkeyTree -> String -> MonkeyTree
getMonkeyTreeNode mm s = case M.lookup s mm of
    Just m -> m
    Nothing -> error (s ++ " should exist in monkey tree map already")

-- this is pretty hacky. what I should do is write new parsing functions and types to create the tree data structure.
-- but I'm a little lazy to write all that stuff after already writing it a certain way for part 1.
calculateMonkeyTree :: [(String, Monkey)] -> M.Map String MonkeyTree -> M.Map String MonkeyTree
calculateMonkeyTree [] mm = mm
calculateMonkeyTree ((n, I i):ms) mm =
    if n == "humn" then calculateMonkeyTree ms (M.insert n Var mm)
    else calculateMonkeyTree ms (M.insert n (Leaf i) mm)
calculateMonkeyTree ((n, M o (x, y)):ms) mm =
    if M.member x mm && M.member y mm then
        if n == "root" then calculateMonkeyTree ms (M.insert n (Root (getMonkeyTreeNode mm x) (getMonkeyTreeNode mm y)) mm)
        else (let mt = case (getMonkeyTreeNode mm x, getMonkeyTreeNode mm y) of
                        (Leaf i, Leaf j) -> Leaf (mOp o i j)
                        (mx, my) -> Node o mx my
            in calculateMonkeyTree ms (M.insert n mt mm))
    else calculateMonkeyTree (ms ++ [(n, M o (x, y))]) mm

reverseRootToHumn :: MonkeyTree -> Integer
reverseRootToHumn (Root (Leaf i) (Node o x y)) = reverseRootToHumn' (Node o x y) i
reverseRootToHumn (Root (Node o x y) (Leaf i)) = reverseRootToHumn' (Node o x y) i
reverseRootToHumn mt = error ("function should start from properly formed root, not any other node: " ++ show mt)

reverseRootToHumn' :: MonkeyTree -> Integer -> Integer
reverseRootToHumn' Var i = i
reverseRootToHumn' (Node o (Leaf l) x) i = case o of
    Add -> reverseRootToHumn' x (i - l)
    Mul -> reverseRootToHumn' x (i `div` l)
    Sub -> reverseRootToHumn' x (l - i)
    Div -> reverseRootToHumn' x (l `div` i)
reverseRootToHumn' (Node o x (Leaf l)) i = case o of
    Add -> reverseRootToHumn' x (i - l)
    Mul -> reverseRootToHumn' x (i `div` l)
    Sub -> reverseRootToHumn' x (i + l)
    Div -> reverseRootToHumn' x (i * l)
reverseRootToHumn' x y = error ("cannot reverse tree from: " ++ show x ++ ", " ++ show y)

solve2 :: [(String, Monkey)] -> Integer
solve2 ms = reverseRootToHumn mt
    where mt = case M.lookup "root" (calculateMonkeyTree ms M.empty) of
                Just r -> r
                Nothing -> error "could not find root"
