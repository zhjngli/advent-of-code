module Y2021.Day16.Solution where

import Data.List

solve :: IO ()
solve = do
    input <- readFile "./src/Y2021/Day16/input.txt"
    let bits = map (concatMap fromHex) $ lines input
    putStrLn $ "2021.16.1: " ++ show (solve1 bits)
    putStrLn $ "2021.16.2: " ++ show (solve2 bits)

fromHex :: Char -> [Int]
fromHex '0' = [0, 0, 0, 0]
fromHex '1' = [0, 0, 0, 1]
fromHex '2' = [0, 0, 1, 0]
fromHex '3' = [0, 0, 1, 1]
fromHex '4' = [0, 1, 0, 0]
fromHex '5' = [0, 1, 0, 1]
fromHex '6' = [0, 1, 1, 0]
fromHex '7' = [0, 1, 1, 1]
fromHex '8' = [1, 0, 0, 0]
fromHex '9' = [1, 0, 0, 1]
fromHex 'A' = [1, 0, 1, 0]
fromHex 'B' = [1, 0, 1, 1]
fromHex 'C' = [1, 1, 0, 0]
fromHex 'D' = [1, 1, 0, 1]
fromHex 'E' = [1, 1, 1, 0]
fromHex 'F' = [1, 1, 1, 1]
fromHex _ = error "unrecognized hex char"

data Packet = Literal Int Int Int | Operator Int Int [Packet] deriving Show

bitsToDec :: [Int] -> Int
bitsToDec = foldl' (\s b -> s * 2 + b) 0

parseLiteral :: [Int] -> (Int, [Int])
parseLiteral bs = (bitsToDec nbs, leftover)
    where (nbs, leftover) = parse bs []
          parse (0:b) n = (n ++ take 4 b, drop 4 b)
          parse (1:b) n = parse (drop 4 b) (n ++ take 4 b)
          parse _ n = error "unrecognized literal bit"

parseOperator :: [Int] -> ([Packet], [Int])
parseOperator (0:bs) = (parse' [] $ take l bs', drop l bs')
    where (l, bs') = (bitsToDec $ take 15 bs, drop 15 bs)
          parse' ps [] = ps
          parse' ps leftover = parse' (ps ++ [p]) leftover'
              where (p, leftover') = parsePacket leftover
parseOperator (1:bs) = parse' n [] leftover
    where (n, leftover) = (bitsToDec $ take 11 bs, drop 11 bs)
          parse' 0 ps leftoverBits = (ps, leftoverBits)
          parse' n ps leftoverBits = parse' (n-1) (ps ++ [p]) lbs'
              where (p, lbs') = parsePacket leftoverBits
parseOperator _ = error "unrecognized bit length type ID"

parsePacket :: [Int] -> (Packet, [Int])
parsePacket bs = (p, leftover)
    where (p, leftover) = if t == 4 then let (ps, bs''') = parseLiteral bs'' in (Literal v t ps, bs''')
                          else let (ps, bs''') = parseOperator bs'' in (Operator v t ps, bs''')
          (v, bs') = (bitsToDec $ take 3 bs, drop 3 bs)
          (t, bs'') = (bitsToDec $ take 3 bs', drop 3 bs')

solve1 :: [[Int]] -> [Int]
solve1 bs = map sumVersion ps
    where ps = map (fst . parsePacket) bs
          sumVersion (Literal v _ _) = v
          sumVersion (Operator v _ ps) = v + sum (map sumVersion ps)

solve2 :: [[Int]] -> [Int]
solve2 bs = map operate packets
    where packets = map (fst . parsePacket) bs
          operate (Operator v 0 ps) = sum $ map operate ps
          operate (Operator v 1 ps) = product $ map operate ps
          operate (Operator v 2 ps) = minimum $ map operate ps
          operate (Operator v 3 ps) = maximum $ map operate ps
          operate (Literal v 4 n) = n
          operate (Operator v 5 ps) = if f > s then 1 else 0
              where f = operate $ head ps
                    s = operate $ last ps
          operate (Operator v 6 ps) = if f < s then 1 else 0
              where f = operate $ head ps
                    s = operate $ last ps
          operate (Operator v 7 ps) = if f == s then 1 else 0
              where f = operate $ head ps
                    s = operate $ last ps
          operate _ = error "unrecognized packet type"
