module Day19.Solution (solve) where

import Text.ParserCombinators.Parsec
import qualified Data.Map.Strict as M

solve :: IO ()
solve = do
    input <- readFile "./src/Day19/input.txt"
    putStrLn $ "2023.19.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.19.2: " ++ show (solve2 $ parseInput input)

type Part = (Int, Int, Int, Int)
data Rule = Go String | G Char Int String | L Char Int String deriving (Show, Eq)
type Workflow = [Rule]

parseParts :: CharParser st [Part]
parseParts = endBy1 parsePart newline
    where
        parsePart = do
            _ <- string "{x="
            x <- many1 digit
            _ <- string ",m="
            m <- many1 digit
            _ <- string ",a="
            a <- many1 digit
            _ <- string ",s="
            s <- many1 digit
            _ <- string "}"
            return (read x, read m, read a, read s)

parseWorkflows :: CharParser st [(String, Workflow)]
parseWorkflows = endBy1 parseWorkflow newline
    where
        parseGoRule = do
            s <- many1 letter
            return $ Go s
        parseGTRule = do
            c <- letter
            _ <- char '>'
            n <- many1 digit
            _ <- char ':'
            s <- many1 letter
            return $ G c (read n) s
        parseLTRule = do
            c <- letter
            _ <- char '<'
            n <- many1 digit
            _ <- char ':'
            s <- many1 letter
            return $ L c (read n) s
        parseWorkflow = do
            name <- many1 letter
            _ <- char '{'
            rules <- sepBy1 (try parseGTRule <|> try parseLTRule <|> try parseGoRule) (char ',')
            _ <- char '}'
            return (name, rules)

inputParser :: CharParser st (M.Map String Workflow, [Part])
inputParser = do
    ws <- parseWorkflows
    _ <- newline
    ps <- parseParts
    return (M.fromList ws, ps)

parseInput :: String -> (M.Map String Workflow, [Part])
parseInput i = case parse inputParser "" i of
    Left e -> error (show e)
    Right x -> x

apply :: Part -> Rule -> Maybe String
apply (x, m, a, s) r = case r of
    Go w -> Just w
    G 'x' n w -> if x > n then Just w else Nothing
    G 'm' n w -> if m > n then Just w else Nothing
    G 'a' n w -> if a > n then Just w else Nothing
    G 's' n w -> if s > n then Just w else Nothing
    L 'x' n w -> if x < n then Just w else Nothing
    L 'm' n w -> if m < n then Just w else Nothing
    L 'a' n w -> if a < n then Just w else Nothing
    L 's' n w -> if s < n then Just w else Nothing
    _ -> error ("unknown rule: " ++ show r)

testPart :: M.Map String Workflow -> Part -> Bool
testPart m part = testPart' part (m M.! "in")
    where
        testPart' p rs = case apply p (head rs) of
            Just "A" -> True
            Just "R" -> False
            Just s -> testPart' p (m M.! s)
            Nothing -> testPart' p (tail rs)

partToInt :: Part -> Int
partToInt (x, m, a, s) = x + m + a + s

solve1 :: (M.Map String Workflow, [Part]) -> Int
solve1 (m, ps) = sum . map partToInt $ filter (testPart m) ps

type PartRange = ((Int, Int), (Int, Int), (Int, Int), (Int, Int))
-- after applying a rule to a range, i get cases:
-- 1. go to something
-- 2. gt. split part range, first one goes somewhere, second continues. OR no split and go somewhere/continue
-- 3. lt. split part range, first one goes somewhere, second continues. OR no split and go somewhere/continue
data RangeResult = RangeSplit PartRange String PartRange | RangeGo String | RangeContinue

-- there's probably a more functional way of doing this but this is readable i guess?
applyRange :: PartRange -> Rule -> RangeResult
applyRange (x@(minx, maxx), m@(minm, maxm), a@(mina, maxa), s@(mins, maxs)) r = case r of
    Go w -> RangeGo w
    G 'x' n w ->
        if minx > n then RangeGo w
        else if n > maxx then RangeContinue
        else RangeSplit ((n+1, maxx), m, a, s) w ((minx, n), m, a, s)
    G 'm' n w ->
        if minm > n then RangeGo w
        else if n > maxm then RangeContinue
        else RangeSplit (x, (n+1, maxm), a, s) w (x, (minm, n), a, s)
    G 'a' n w ->
        if mina > n then RangeGo w
        else if n > maxa then RangeContinue
        else RangeSplit (x, m, (n+1, maxa), s) w (x, m, (mina, n), s)
    G 's' n w ->
        if mins > n then RangeGo w
        else if n > maxs then RangeContinue
        else RangeSplit (x, m, a, (n+1, maxs)) w (x, m, a, (mins, n))
    L 'x' n w ->
        if maxx < n then RangeGo w
        else if n < minx then RangeContinue
        else RangeSplit ((minx, n-1), m, a, s) w ((n, maxx), m, a, s)
    L 'm' n w ->
        if maxm < n then RangeGo w
        else if n < minm then RangeContinue
        else RangeSplit (x, (minm, n-1), a, s) w (x, (n, maxm), a, s)
    L 'a' n w ->
        if maxa < n then RangeGo w
        else if n < mina then RangeContinue
        else RangeSplit (x, m, (mina, n-1), s) w (x, m, (n, maxa), s)
    L 's' n w ->
        if maxs < n then RangeGo w
        else if n < mins then RangeContinue
        else RangeSplit (x, m, a, (mins, n-1)) w (x, m, a, (n, maxs))
    _ -> error ("unknown rule: " ++ show r)

partsInRange :: PartRange -> Int
partsInRange ((minx, maxx), (minm, maxm), (mina, maxa), (mins, maxs)) = (maxx - minx + 1) * (maxm - minm + 1) * (maxa - mina + 1) * (maxs - mins + 1)

testPartRange :: M.Map String Workflow -> PartRange -> Int
testPartRange m partrange = testPart' partrange (m M.! "in")
    where
        testPart' p rs = case applyRange p (head rs) of
            RangeGo "A" -> partsInRange p
            RangeGo "R" -> 0
            RangeGo s -> testPart' p (m M.! s)
            RangeSplit p1 "A" p2 -> partsInRange p1 + testPart' p2 (tail rs)
            RangeSplit _  "R" p2 -> 0 + testPart' p2 (tail rs)
            RangeSplit p1 s   p2 -> testPart' p1 (m M.! s) + testPart' p2 (tail rs)
            RangeContinue -> testPart' p (tail rs)

solve2 :: (M.Map String Workflow, [Part]) -> Int
solve2 (m, _) = testPartRange m ((1, 4000), (1, 4000), (1, 4000), (1, 4000))
