module Y2021.Day14.Solution where

import Data.List
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day14/input.txt"
    putStrLn $ "2021.14.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.14.2: " ++ show (solve2 $ parseInput input)

type Rules = M.Map (Char, Char) Char
type Pairs = M.Map (Char, Char) Int
type Counts = M.Map Char Int

parsePolymer :: CharParser st String
parsePolymer = do
    p <- many1 upper
    newline
    return p

parseRule :: CharParser st (Char, Char, Char)
parseRule = do
    a <- upper
    b <- upper
    string " -> "
    c <- upper
    return (a, b, c)

parseLines :: CharParser st (String, [(Char, Char, Char)])
parseLines = do
    p <- parsePolymer
    newline
    rules <- endBy parseRule newline
    return (p, rules)

countMap :: Ord a => [a] -> M.Map a Int
countMap = foldr count M.empty
    where count a m = if M.member a m then M.adjust (+1) a m else M.insert a 1 m

parseInput :: String -> (Pairs, Counts, Rules)
parseInput i = case parse parseLines "" i of
    Left e -> (M.empty, M.empty, M.empty)
    Right (p, ruleChars) -> (pairs, counts, rules)
        where rules = foldr (\(a, b, c) m -> M.insert (a, b) c m) M.empty ruleChars
              pairs = countMap . snd $ foldl' pair (head p, []) (tail p)
                  where pair (prev, acc) c = (c, (prev, c):acc)
              counts = countMap p

pairInsert :: Rules -> Pairs -> Counts -> (Pairs, Counts)
pairInsert r p c = (np, M.unionWith (+) c nc)
    where (np, nc) = M.foldrWithKey ins (M.empty, M.empty) r
          ins (c1, c2) i (anp, anc) = if rulePairs == 0 then (anp, anc) else (addC2Pair, addCount)
              where rulePairs = M.findWithDefault 0 (c1, c2) p
                    addCount = if M.member i anc then M.adjust (+rulePairs) i anc else M.insert i rulePairs anc
                    addC1Pair = if M.member (c1, i) anp then M.adjust (+rulePairs) (c1, i) anp else M.insert (c1, i) rulePairs anp
                    addC2Pair = if M.member (i, c2) addC1Pair then M.adjust (+rulePairs) (i, c2) addC1Pair else M.insert (i, c2) rulePairs addC1Pair

countPolymer :: Int -> (Pairs, Counts, Rules) -> Int
countPolymer n (p, c, r) = (snd . last) resCounts - (snd . head) resCounts
    where step 0 r p c = c
          step x r p c = step (x-1) r p' c'
              where (p', c') = pairInsert r p c
          resCounts = sortBy (comparing snd) $ M.toList $ step n r p c

solve1 :: (Pairs, Counts, Rules) -> Int
solve1 = countPolymer 10

solve2 :: (Pairs, Counts, Rules) -> Int
solve2 = countPolymer 40
