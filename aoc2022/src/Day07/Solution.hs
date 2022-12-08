module Day07.Solution (solve) where

-- import Debug.Trace
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day07/input.txt"
    putStrLn $ "2022.07.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.07.2: " ++ show (solve2 $ parseInput input)

data LSOut = D String | F String Int deriving (Show, Eq)
data Cmd = Up | CD String | LS [LSOut] deriving (Show, Eq)

parseUp :: CharParser st Cmd
parseUp = do
    _ <- try $ string " cd .."
    _ <- newline
    return Up

parseCD :: CharParser st Cmd
parseCD = do
    _ <- try $ string " cd "
    dir <- string "/" <|> many1 letter
    _ <- newline
    return $ CD dir

parseLSDir :: CharParser st LSOut
parseLSDir = do
    _ <- try $ string "dir "
    d <- many1 letter
    return $ D d

parseLSFile :: CharParser st LSOut
parseLSFile = do
    s <- try $ many1 digit
    _ <- char ' '
    n <- many1 (letter <|> char '.')
    return $ F n (read s)

parseLS :: CharParser st Cmd
parseLS = do
    _ <- try $ string " ls"
    _ <- newline
    ls <- try $ endBy1 (parseLSDir <|> parseLSFile) newline
    return $ LS ls

parseAction :: CharParser st Cmd
parseAction = parseUp <|> parseCD <|> parseLS

parseActions :: [String] -> [Cmd]
parseActions = map f . filter (not . null)
    where f s = case parse parseAction "" s of
                Left e -> error (show e)
                Right cmd -> cmd

split :: Char -> String -> [String]
split _ [] = [""]
split delim (c:cs)
    | c == delim = "" : rest
    | otherwise = (c : head rest) : tail rest
    where rest = split delim cs

parseInput :: String -> [Cmd]
parseInput = parseActions . split '$'

data FS = File String Int | Dir String [FS] deriving (Show, Eq)
data FSCrumb = FSCrumb String [FS] [FS] deriving Show
type FSZipper = (FS, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, (FSCrumb name lefts rights):crumbs) = (Dir name (lefts ++ [item] ++ rights), crumbs)
fsUp (item, []) = (item, []) -- assumes this item is (Dir "/" [...])

fsUpmost :: FSZipper -> FSZipper
fsUpmost (item, []) = (item, [])
fsUpmost fsz = fsUpmost (fsUp fsz)

fsTo :: String -> FSZipper -> FSZipper
fsTo name (Dir dir items, bs) = (item, FSCrumb dir lefts rights:bs)
    where (lefts, item:rights) = break (nameIs name) items
fsTo _ (File n _, _) = error ("cannot cd into file: " ++ n)

nameIs :: String -> FS -> Bool
nameIs name (Dir dir _)   = name == dir
nameIs name (File file _) = name == file

lsOutToFS :: LSOut -> FS
lsOutToFS (D n) = Dir n [] -- assumes no LS is performed on a previously discovered directory
lsOutToFS (F n s) = File n s

cmdsToFS :: [Cmd] -> FS
cmdsToFS = fst . cmdsToFS' (Dir "/" [], [])

cmdsToFS' :: FSZipper -> [Cmd] -> FSZipper
cmdsToFS' fsz (CD "/":cmds) = cmdsToFS' (fsUpmost fsz) cmds
cmdsToFS' fsz (CD d:cmds) = cmdsToFS' (fsTo d fsz) cmds
cmdsToFS' (Dir n fs, crumbs) (LS lsouts:cmds) = cmdsToFS' (Dir n (map lsOutToFS lsouts ++ fs), crumbs) cmds
cmdsToFS' (File n _, _) (LS _:_) = error ("cannot ls a file: " ++ n)
cmdsToFS' fsz (Up:cmds) = cmdsToFS' (fsUp fsz) cmds
cmdsToFS' fsz [] = fsUpmost fsz

fsSize :: FS -> Int
fsSize (File _ s) = s
fsSize (Dir _ fss) = sum $ map fsSize fss

dirSizes :: [FS] -> [Int] -> [Int]
dirSizes [] sizes = sizes
dirSizes (d@(Dir _ is):fss) sizes = dirSizes (is ++ fss) (fsSize d:sizes)
dirSizes (File _ _:fss) sizes = dirSizes fss sizes

solve1 :: [Cmd] -> Int
solve1 cmds = sum . filter (<= 100000) $ dirSizes [cmdsToFS cmds] []

solve2 :: [Cmd] -> Int
solve2 cmds = minimum . filter (>= minSpaceToFree) $ dirSizes [fs] []
    where fs = cmdsToFS cmds
          diskSpace = 70000000
          neededSpace = 30000000
          fsSpace = fsSize fs
          unusedSpace = diskSpace - fsSpace
          minSpaceToFree = neededSpace - unusedSpace
