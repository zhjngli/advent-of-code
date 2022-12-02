module Y2021.Day06.Solution where

import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day06/input.txt"
    putStrLn $ "2021.06.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.06.2: " ++ show (solve2 $ parseInput input)

parseFish :: CharParser st [Int]
parseFish = do
    fish <- sepBy (many1 digit) (char ',')
    newline
    return $ map read fish

-- map timer to counts
parseInput :: String -> M.Map Int Int
parseInput i = case parse parseFish "" i of
    Left e -> M.empty
    Right f -> foldr count M.empty f
        where count p m = if M.member p m then M.adjust (+1) p m else M.insert p 1 m

decreaseTimer :: M.Map Int Int -> M.Map Int Int
decreaseTimer = M.foldrWithKey dec M.empty
    where dec 0 c m =
            let m6 = if M.member 6 m then M.adjust (+c) 6 m else M.insert 6 c m in
            if M.member 8 m6 then M.adjust (+c) 8 m6 else M.insert 8 c m6
          dec t c m = if M.member (t-1) m then M.adjust (+c) (t-1) m else M.insert (t-1) c m
            
decreaseXDays :: Int -> M.Map Int Int -> M.Map Int Int
decreaseXDays 0 m = m
decreaseXDays i m = decreaseXDays (i-1) (decreaseTimer m)

solve1 :: M.Map Int Int -> Int
solve1 m = M.foldr (+) 0 (decreaseXDays 80 m)

solve2 :: M.Map Int Int -> Int
solve2 m = M.foldr (+) 0 (decreaseXDays 256 m)
