module Y2021.Day17.Solution where

import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day17/input.txt"
    putStrLn $ "2021.17.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.17.2: " ++ show (solve2 $ parseInput input)

data Target = T (Int, Int) (Int, Int) deriving Show

parseTarget :: CharParser st Target
parseTarget = do
    string "target area: x="
    minX <- many1 digit
    string ".."
    maxX <- many1 digit
    string ", y=-"
    minY <- many1 digit
    string "..-"
    maxY <- many1 digit
    newline
    return $ T (read minX, read maxX) (-(read minY), -(read maxY))

parseInput :: String -> Target
parseInput i = case parse parseTarget "" i of
    Left e -> T (0, 0) (0, 0)
    Right t -> t

inTarget :: (Int, Int) -> Target -> Bool
inTarget (x, y) (T (sx, bx) (sy, by)) = x >= sx && x <= bx && y >= sy && y <= by

pastTarget :: (Int, Int) -> Target -> Bool
pastTarget (x, y) t@(T (sx, bx) (sy, by)) = x > bx || y < sy

hitsTarget :: Target -> [(Int, Int)] -> [((Int, Int), (Int, Int))] -> (Bool, [(Int, Int)])
hitsTarget t ps [] = error "give me an infinite list bro"
hitsTarget t ps ((v, p):vps)
    | pastTarget p t = (False, p:ps)
    | inTarget p t = (True, p:ps)
    | otherwise = hitsTarget t (p:ps) vps

step :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
step ((vx, vy), (x, y)) = ((max 0 (vx - 1), vy - 1), (x + vx, y + vy))

solve1 :: Target -> Int
solve1 t@(T (sx, bx) (sy, by)) = maxY
    where tries = [iterate step ((x, y), (0, 0)) | x <- [1 .. bx], y <- [1 .. abs sy]]
          successes = filter fst $ map (hitsTarget t []) tries
          pos = map snd successes
          maxY = maximum $ map (maximum . map snd) pos

solve2 :: Target -> Int
solve2 t@(T (sx, bx) (sy, by)) = length successes
    where tries = [iterate step ((x, y), (0, 0)) | x <- [1 .. bx], y <- [sy .. abs sy]]
          successes = filter fst $ map (hitsTarget t []) tries
