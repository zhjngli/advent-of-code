module Day07.Solution (solve) where

import Data.List.Split
import Data.List ( foldl', sort, sortBy )
import Data.Map as M (elems, empty, delete, insertWith, findWithDefault, Map)

solve :: IO ()
solve = do
    input <- readFile "./src/Day07/input.txt"
    putStrLn $ "2023.07.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.07.2: " ++ show (solve2 $ parseInput input)

data Card = N Int | T | J | Q | K | A deriving (Show, Eq, Ord)
data Hand = High | Pair | TwoPair | ThreeKind | FullHouse | FourKind | FiveKind deriving (Show, Eq, Ord)

parseHand :: String -> [Card]
parseHand = foldr
    (\c cs ->
        case c of
        'A' -> A:cs
        'K' -> K:cs
        'Q' -> Q:cs
        'J' -> J:cs
        'T' -> T:cs
        n -> N (read [n]) :cs
    )
    []

parseInput :: String -> [([Card], Int)]
parseInput i = foldl'
    (\a l ->
        case splitOn " " l of
        [h, b] -> (parseHand h, read b):a
        _ -> error "improper format"
    )
    [] (lines i)

countItems :: Ord a => [a] -> M.Map a Int
countItems = foldl'
    (\count c -> M.insertWith (+) c (1::Int) count)
    M.empty

getHand' :: ([Card] -> [Int]) -> [Card] -> Hand
getHand' toCounts cs = case toCounts cs of
    [5] -> FiveKind
    [1, 4] -> FourKind
    [2, 3] -> FullHouse
    [1, 1, 3] -> ThreeKind
    [1, 2, 2] -> TwoPair
    [1, 1, 1, 2] -> Pair
    [1, 1, 1, 1, 1] -> High
    _ -> error ("improper hand: " ++ show cs)

handToCounts1 :: [Card] -> [Int]
handToCounts1 cs = sort $ M.elems counter
    where counter = countItems cs

getHand1 :: [Card] -> Hand
getHand1 = getHand' handToCounts1

handToCounts2 :: [Card] -> [Int]
handToCounts2 cs = initCounts ++ [lastCounts + jokers]
    where
        (initCounts, lastCounts) = case counts of
            [] -> ([], 0) -- handles case where counts is empty, i.e. 5 of a kind jokers)
            _ -> (init counts, last counts)
        counts = sort $ M.elems counterNoJokers
        jokers = M.findWithDefault 0 J counter
        counterNoJokers = M.delete J counter
        counter = countItems cs

getHand2 :: [Card] -> Hand
getHand2 = getHand' handToCounts2

compByCard2 :: [Card] -> [Card] -> Ordering
compByCard2 (c1:cs1') (c2:cs2') =
    if c1 == c2 then compByCard2 cs1' cs2'
    else
        case (c1, c2) of
        (J, _) -> LT
        (_, J) -> GT
        _ -> compare c1 c2
compByCard2 [] [] = EQ
compByCard2 cs1 cs2 = error ("inequal lengths, hand 1: " ++ show cs1 ++ ", " ++ "hand 2: " ++ show cs2)

compareCards :: ([Card] -> Hand) -> ([Card] -> [Card] -> Ordering) -> [Card] -> [Card] -> Ordering
compareCards getHand compareCards' cs1 cs2 =
    let h1 = getHand cs1
        h2 = getHand cs2
    in
    if h1 == h2 then compareCards' cs1 cs2
    else compare h1 h2

solve1 :: [([Card], Int)] -> Int
solve1 = solve' getHand1 compare

solve2 :: [([Card], Int)] -> Int
solve2 = solve' getHand2 compByCard2

solve' :: ([Card] -> Hand) -> ([Card] -> [Card] -> Ordering) -> [([Card], Int)] -> Int
solve' getHand compareCards' cbs = (sum . map (\((_, b), r) -> b * r)) withRank
    where sorted = sortBy (\(c1, _) (c2, _) -> compareCards getHand compareCards' c1 c2) cbs
          withRank = zip sorted [1..]
