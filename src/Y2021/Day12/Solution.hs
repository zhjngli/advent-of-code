module Y2021.Day12.Solution where

import Data.Char
import qualified Data.Map.Strict as MS
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec hiding (Line)

solve = do
    input <- readFile "./src/Y2021/Day12/input.txt"
    putStrLn $ "2021.12.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.12.2: " ++ show (solve2 $ parseInput input)

data Node = Start | Big String | Small String | End deriving (Show, Ord, Eq)

toNode :: String -> Node
toNode "start" = Start
toNode "end" = End
toNode s | all isUpper s = Big s
         | all isLower s = Small s
         | otherwise = error $ "invalid node: " ++ s

parseLine :: CharParser st (Node, Node)
parseLine = do
    begin <- many1 letter
    char '-'
    end <- many1 letter
    newline
    return (toNode begin, toNode end)

parseLines :: CharParser st [(Node, Node)]
parseLines = many parseLine

parseInput :: String -> [(Node, Node)]
parseInput i = case parse parseLines "" i of
    Left e -> []
    Right lines -> lines

graph :: [(Node, Node)] -> M.Map Node [Node]
graph nns = graph' nns M.empty
    where graph' ((n1, n2):ns) m = graph' ns (MS.insertWith (++) n2 [n1] $ MS.insertWith (++) n1 [n2] m)
          graph' [] m = m

canVisit1 :: M.Map Node Int -> Node -> Bool
canVisit1 _ Start = False
canVisit1 _ (Big _) = True
canVisit1 v n = M.findWithDefault 0 n v < 1

canVisit2 :: M.Map Node Int -> Node -> Bool
canVisit2 _ Start = False
canVisit2 _ (Big _) = True
canVisit2 v n = M.findWithDefault 0 n v < smallVisitLimit
    where smallVisitLimit = if smallVisitedTwice then 1 else 2
          smallVisitedTwice = any ((== 2) . snd) $ filter smallCave (M.assocs v)
          smallCave (Small _, _) = True
          smallCave _ = False

findPaths :: (M.Map Node Int -> Node -> Bool) -> M.Map Node [Node] -> [Node] -> M.Map Node Int -> S.Set [Node]
findPaths _ g [] v = S.empty
findPaths _ g path@(End:ns) v = S.singleton path
findPaths canVisit g path@(n:ns)   v = S.unions nextPaths
    where nextNodes = filter (canVisit v) $ M.findWithDefault [] n g
          visit n = if M.member n v then M.adjust (+1) n v else M.insert n 1 v
          nextPaths = map (\n -> findPaths canVisit g (n:path) (visit n)) nextNodes

solve1 :: [(Node, Node)] -> Int
solve1 ns = S.size $ findPaths canVisit1 g [Start] (M.singleton Start 1)
    where g = graph ns

solve2 :: [(Node, Node)] -> Int
solve2 ns = S.size $ findPaths canVisit2 g [Start] (M.singleton Start 1)
    where g = graph ns
