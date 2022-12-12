module Day10.Solution (solve) where

-- import Debug.Trace
import Data.List ( foldl' )
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day10/input.txt"
    putStrLn $ "2022.10.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.10.2: " ++ solve2 (parseInput input)

data Cmd = Noop | Add Int deriving Show

parseNoop :: CharParser st Cmd
parseNoop = do
    _ <- string "noop"
    return Noop

parseAdd :: CharParser st Cmd
parseAdd = do
    _ <- string "addx "
    n <- many1 (digit <|> char '-')
    return $ Add (read n)

parseCmds :: CharParser st [Cmd]
parseCmds = endBy1 (parseAdd <|> parseNoop) newline

parseInput :: String -> [Cmd]
parseInput i = case parse parseCmds "" i of
    Left e -> error (show e)
    Right cmds -> cmds

calcSignalStrength :: [(Int, Int)] -> Int
calcSignalStrength rs = sum $ map calc [(r220, 220), (r180, 180), (r140, 140), (r100, 100), (r60, 60), (r20, 20)]
    where (_, r220) = break ((<= 220) . snd) rs
          (_, r180) = break ((<= 180) . snd) r220
          (_, r140) = break ((<= 140) . snd) r180
          (_, r100) = break ((<= 100) . snd) r140
          (_, r60) = break ((<= 60) . snd) r100
          (_, r20) = break ((<= 20) . snd) r60
          calc ((r, _):_, n) = r * n
          calc ([], _) = error "calc should never get empty list"

regsOverTime :: [Cmd] -> [(Int, Int)]
regsOverTime = foldl' f [(1, 1)] -- register holds value at beginning of cycle
    where f ((x, c):cs) Noop    = (  x, c+1):(x, c):cs
          f ((x, c):cs) (Add n) = (x+n, c+2):(x, c):cs
          f [] _ = error "should never happen"

solve1 :: [Cmd] -> Int
solve1 = calcSignalStrength . regsOverTime

getPix :: Int -> Int -> Char
getPix x c = if abs (c - x) <= 1 then '#' else ' '

showCRT :: [(Int, Int)] -> String
showCRT xs = fst3 $ foldl' showRow ("\n", 1, xs) [0..5]
    where fst3 (s, _, _) = s
          showRow (crt, regVal, regVals) row =
            let (rowToShow, x', xs') = foldl' showPix ("", regVal, regVals) [0..39] in
            (crt ++ reverse rowToShow ++ "\n", x', xs')
            where showPix (s, x, (nextX, c):regs) col =
                    let cycleN = row * 40 + col + 1 in
                    if cycleN >= c then (getPix nextX col:s, nextX, regs)
                    else (getPix x col:s, x, (nextX, c):regs)
                  showPix (_, _, []) _ = error "showPix should never reach empty cycles"

solve2 :: [Cmd] -> String
solve2 = showCRT . reverse . regsOverTime
