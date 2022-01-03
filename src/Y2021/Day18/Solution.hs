module Y2021.Day18.Solution where

import qualified Control.Applicative as A
import Data.List
import Text.ParserCombinators.Parsec hiding (Line)

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day18/input.txt"
    putStrLn $ "2021.18.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2021.18.2: " ++ show (solve2 $ parseInput input)

data Snail = S Snail Snail | I Int deriving (Show, Eq)
data SnailC = Top | L SnailC Snail | R Snail SnailC deriving Show
type SnailLoc = (Snail, SnailC)

parseNum :: CharParser st Snail
parseNum = do
    n <- many1 digit
    return (I $ read n)

parseSnail :: CharParser st Snail
parseSnail = do
    char '['
    left <- parseSnail <|> parseNum
    char ','
    right <- parseSnail <|> parseNum
    char ']'
    return $ S left right

parseLines :: CharParser st [Snail]
parseLines = endBy parseSnail newline

parseInput :: String -> [Snail]
parseInput i = case parse parseLines "" i of
    Left e -> []
    Right snails -> snails

left :: SnailLoc -> SnailLoc
left (S l r, c) = (l, L c r)
left _ = error "can't take the left of this snail"

right :: SnailLoc -> SnailLoc
right (S l r, c) = (r, R l c)
right _ = error "can't take the right of this snail"

top :: Snail -> SnailLoc
top s = (s, Top)

up :: SnailLoc -> SnailLoc
up (s, L c r) = (S s r, c)
up (s, R l c) = (S l s, c)
up (s, Top) = (s, Top)

upmost :: SnailLoc -> SnailLoc
upmost (s, Top) = (s, Top)
upmost sl = upmost (up sl)

leftmost :: SnailLoc -> SnailLoc
leftmost sc@(I n, c) = sc
leftmost sc@(S l r, c) = leftmost $ left sc

rightmost :: SnailLoc -> SnailLoc
rightmost sc@(I n, c) = sc
rightmost sc@(S l r, c) = rightmost $ right sc

modify :: SnailLoc -> (Snail -> Snail) -> SnailLoc
modify (s, c) f = (f s, c)

explode :: Snail -> Maybe Snail
explode s = case findPairToExplode 4 (top s) >>= explode' of
    Nothing -> Nothing
    Just (s', c) -> Just s'

snailToRight :: SnailLoc -> Maybe SnailLoc
snailToRight (_, Top) = Nothing
snailToRight sc@(s, L c r) = Just $ leftmost $ right $ up sc
snailToRight sc@(s, R l c) = snailToRight $ up sc

snailToLeft :: SnailLoc -> Maybe SnailLoc
snailToLeft (_, Top) = Nothing
snailToLeft sc@(s, L c r) = snailToLeft $ up sc
snailToLeft sc@(s, R l c) = Just $ rightmost $ left $ up sc

findPairToExplode :: Int -> SnailLoc -> Maybe SnailLoc
findPairToExplode 0 sc@(I n, c)           = Nothing
findPairToExplode 0 sc@(S (I l) (I r), c) = Just sc
findPairToExplode 0 sc@(S l r, c)         = error "snail numbers must be reduced"
findPairToExplode d sc@(I n, c)           = Nothing
findPairToExplode d sc@(S l r, c)         = findPairToExplode (d-1) (left sc) A.<|> findPairToExplode (d-1) (right sc)

explode' :: SnailLoc -> Maybe SnailLoc
explode' sc@(S (I l) (I r), c) = Just replaceWith0sc
    where leftModifiedsc = case snailToLeft sc of
              Nothing -> upmost sc
              Just (I n, c) -> upmost (I (n + l), c)
              _ -> error "can't find snail to the left"
          rightModifiedsc = case findPairToExplode 4 leftModifiedsc >>= snailToRight of
              Nothing -> upmost leftModifiedsc
              Just (I n, c) -> upmost (I (n + r), c)
              _ -> error "can't find snail to the right"
          replaceWith0sc = case findPairToExplode 4 rightModifiedsc of
              Just (S l r, c) -> upmost (I 0, c)
              _ -> error "should be found"
explode' _ = error "can't explode non snail pair"

split :: Snail -> Maybe Snail
split s = case splitC $ top s of
    Nothing -> Nothing
    Just (s', c) -> Just s'

splitC :: SnailLoc -> Maybe SnailLoc
splitC sc@(I n, c) | n >= 10 = Just (upmost $ modify sc split')
                   | otherwise = Nothing
                   where split' (I n) = S (I $ n `div` 2) (I $ if even n then n `div` 2 else n `div` 2 + 1)
                         split' _ = error "can't split snail pair"
splitC sc = splitC (left sc) A.<|> splitC (right sc)

reduce :: Snail -> Snail
reduce s = maybe s reduce (explode s A.<|> split s)

snailSum :: Snail -> Snail -> Snail
snailSum l r = reduce (S l r)

magnitude :: Snail -> Int
magnitude (S l r) = 3 * magnitude l + 2 * magnitude r
magnitude (I n) = n

solve1 :: [Snail] -> Int
solve1 s = magnitude $ foldl' snailSum (head s) (tail s)

solve2 :: [Snail] -> Int
solve2 s = maximum [magnitude (snailSum a b) | a <- s, b <- s, a /= b]
