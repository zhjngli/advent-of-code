module Day15.Solution (solve) where

import Data.List.Split
import Data.List (foldl')
import Data.Char
import qualified Data.Map as M

solve :: IO ()
solve = do
    input <- readFile "./src/Day15/input.txt"
    putStrLn $ "2023.15.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.15.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> [String]
parseInput = splitOn ","

hash :: String -> Int
hash = foldl' (\h c -> ((h + ord c) * 17) `mod` 256) 0

solve1 :: [String] -> Int
solve1 = sum . map hash

type Lens = (String, Int)
type Boxes = M.Map Int [Lens]

remove :: Boxes -> String -> Boxes
remove b s = M.insert h ls' b
    where
        h = hash s
        ls = M.findWithDefault [] h b
        ls' = foldr (\lens@(l, _) acc -> if l == s then acc else lens:acc) [] ls

place :: Boxes -> String -> Int -> Boxes
place b s i = M.insert h ls'' b
    where
        h = hash s
        ls = M.findWithDefault [] h b
        (ls', found) = foldr
                (\lens@(l, _) (acc, f) ->
                    if l == s then ((s, i):acc, True)
                    else (lens:acc, f)
                )
                ([], False) ls
        ls'' = if found then ls' else (s, i):ls'

focusingPower :: Boxes -> Int
focusingPower b = (sum . map power) (M.assocs b)
    where
        power (i, ls) = (i+1) * fst (foldr (\(_, l) (total, slot) -> (total + l * slot, slot + 1)) (0, 1) ls)

solve2 :: [String] -> Int
solve2 ss = focusingPower m
    where
        m = foldl'
            (\m' s ->
                case last s of
                '-' -> remove m' (init s)
                -- assume this is a label
                _ -> place m' label focalLength
                    where
                        s' = splitOn "=" s
                        label = head s'
                        focalLength = read $ last s'
            )
            M.empty ss
