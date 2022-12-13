module Day13.Solution (solve) where

-- import Debug.Trace
import Data.List ( foldl', elemIndex, sort )
import Data.Maybe
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day13/input.txt"
    -- test <- readFile "./src/Day13/test.txt"
    putStrLn $ "2022.13.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.13.2: " ++ show (solve2 $ parseInput input)

data PacketData = I Int | L [PacketData] deriving (Show, Eq)
type Packet = [PacketData]

parsePacketList :: CharParser st PacketData
parsePacketList = do
    _ <- char '['
    l <- sepBy (parsePacketList <|> parsePacketInt) (char ',')
    _ <- char ']'
    return $ L l

parsePacketInt :: CharParser st PacketData
parsePacketInt = do
    i <- many1 digit
    return $ I (read i)

parsePacket :: CharParser st Packet
parsePacket = do
    _ <- char '['
    p <- sepBy (parsePacketList <|> parsePacketInt) (char ',')
    _ <- char ']'
    return p

parsePacketPairs :: CharParser st [(Packet, Packet)]
parsePacketPairs = do
    sepBy1 parsePacketPair newline
    where parsePacketPair = do
            p1 <- parsePacket
            _ <- newline
            p2 <- parsePacket
            _ <- newline
            return (p1, p2)

parseInput :: String -> [(Packet, Packet)]
parseInput i = case parse parsePacketPairs "" i of
    Left e -> error (show e)
    Right ps -> ps

comparePacket :: Packet -> Packet -> Maybe Bool
comparePacket (I left:p1s) (I right:p2s)
  | left < right = Just True
  | right < left = Just False
  | otherwise = comparePacket p1s p2s
comparePacket (L left:p1s) (L right:p2s) =
    case comparePacket left right of
    Nothing -> comparePacket p1s p2s
    Just x -> Just x
comparePacket (L left:p1s) (I right:p2s) =
    case comparePacket left [I right] of
    Nothing -> comparePacket p1s p2s
    Just x -> Just x
comparePacket (I left:p1s) (L right:p2s) =
    case comparePacket [I left] right of
    Nothing -> comparePacket p1s p2s
    Just x -> Just x
comparePacket [] [] = Nothing
comparePacket _ [] = Just False
comparePacket [] _ = Just True

solve1 :: [(Packet, Packet)] -> Int
solve1 ps = foldl' f 0 $ zip [1..length ps] ps
    where f s (i, (p1, p2)) =
            case comparePacket p1 p2 of
            Nothing -> error ("comparison should've succeeded\np1: " ++ show p1 ++ "\np2: " ++ show p2)
            Just True -> i + s
            Just False -> s

instance Ord PacketData where
    compare pd1 pd2 = case comparePacket [pd1] [pd2] of
        Nothing -> EQ
        Just True -> LT
        Just False -> GT

solve2 :: [(Packet, Packet)] -> Int
solve2 ps = dp1i * dp2i
    where dividerPacket1 = [L [I 2]]
          dividerPacket2 = [L [I 6]]
          packets = sort $ dividerPacket1:dividerPacket2:concatMap (\(p1, p2) -> [p1, p2]) ps
          dp1i = 1 + fromJust (elemIndex dividerPacket1 packets)
          dp2i = 1 + fromJust (elemIndex dividerPacket2 packets)
