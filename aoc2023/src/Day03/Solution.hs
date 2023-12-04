module Day03.Solution (solve) where

import Data.Array
import Data.List (foldl')
import Data.Map as M (empty, findWithDefault, foldrWithKey, insert)
import Data.Set as S (elemAt, empty, insert, size)
import Lib.Common

solve :: IO ()
solve = do
    input <- readFile "./src/Day03/input.txt"
    putStrLn $ "2023.03.1: " ++ show (solve1 $ (toArray . map (map charToSchematic) . lines) input)
    putStrLn $ "2023.03.2: " ++ show (solve2 $ (toArray . map (map charToSchematic) . lines) input)

data Schematic = N | S | G | D Int

charToSchematic :: Char -> Schematic
charToSchematic '.' = N
charToSchematic '0' = D 0
charToSchematic '1' = D 1
charToSchematic '2' = D 2
charToSchematic '3' = D 3
charToSchematic '4' = D 4
charToSchematic '5' = D 5
charToSchematic '6' = D 6
charToSchematic '7' = D 7
charToSchematic '8' = D 8
charToSchematic '9' = D 9
charToSchematic '*' = G
charToSchematic _ = S

neighbors :: Array (Int, Int) Schematic -> (Int, Int) -> [(Int, Int)]
neighbors a (r, c) = filter (inRange (bounds a)) [(r+x, c+y) | (x, y) <- [(1,0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]]

numberFromStack :: [Schematic] -> Int
numberFromStack [] = 0
numberFromStack (D d:ss) = d + 10 * numberFromStack ss
numberFromStack _ = error "non digit schematic cannot be converted to number"

isSymbol :: Schematic -> Bool
isSymbol S = True
isSymbol G = True
isSymbol _ = False

bordersSymbol :: (Int, Int) -> Array (Int, Int) Schematic -> Bool
bordersSymbol i a = any (isSymbol . (a !)) (neighbors a i)

-- may not work on all inputs cause i assume numbers don't wrap lines
solve1 :: Array (Int, Int) Schematic -> Int
solve1 a = s
    where (s, _, _) = foldl'
            (\(total, borders, digitStack) ((r, c), schematic) ->
                case schematic of
                D _ -> (total, borders || bordersSymbol (r, c) a, schematic:digitStack)
                _ -> (if borders then total + numberFromStack digitStack else total, False, [])
            )
            (0, False, [])
            (assocs a)

isGear :: Schematic -> Bool
isGear G = True
isGear _ = False

bordersGear :: (Int, Int) -> Array (Int, Int) Schematic -> [(Int, Int)]
bordersGear i a = filter (isGear . (a !)) (neighbors a i)

-- assumes each number is bordering at most one gear
solve2 :: Array (Int, Int) Schematic -> Int
solve2 a =
    M.foldrWithKey
        (\k gearNums total ->
            case gearNums of
            [x, y] -> total + x * y
            [_] -> total
            [] -> total
            _ -> error ("more than 2 numbers attached to this gear: " ++ show k ++ ", " ++ show gearNums))
        0 s
    where (s, _, _) = foldl'
            (\(gearMap, borders, digitStack) ((r, c), schematic) ->
                case schematic of
                D _ -> (gearMap, foldl' (flip S.insert) borders (bordersGear (r, c) a), schematic:digitStack)
                _ -> (
                    case S.size borders of
                    0 -> gearMap
                    1 -> let b = S.elemAt 0 borders in M.insert b (numberFromStack digitStack : M.findWithDefault [] b gearMap) gearMap
                    _ -> error ("this number borders more than one gear: " ++ show borders)
                    , S.empty, [])
            )
            (M.empty, S.empty, [])
            (assocs a)
