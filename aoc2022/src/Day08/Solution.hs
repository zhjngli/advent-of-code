module Day08.Solution (solve) where

-- import Debug.Trace
import Data.Array
import Data.Char

solve :: IO ()
solve = do
    input <- readFile "./src/Day08/input.txt"
    let heightMap = map (map digitToInt) (lines input)
    putStrLn $ "2022.08.1: " ++ show (solve1 heightMap)
    putStrLn $ "2022.08.2: " ++ show (solve2 heightMap)

toArray :: [[Int]] -> Array (Int, Int) Int
toArray l = array bound associations
    where r = length l
          c = length $ head l
          bound = ((0, 0), (r-1, c-1))
          cIndices = map (zip [0..c-1]) l
          rcIndices = zip [0..r-1] cIndices
          associations = concatMap associate rcIndices
          associate (r', cs) = map (\(c', i) -> ((r', c'), i)) cs

left :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
left a (r, c) = filter (inRange (bounds a)) [(r+x, c) | x <- [-100..(-1)]]

right :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
right a (r, c) = filter (inRange (bounds a)) (reverse [(r+x, c) | x <- [1..100]])

up :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
up a (r, c) = filter (inRange (bounds a)) [(r, c+y) | y <- [-100..(-1)]]

down :: Array (Int, Int) Int -> (Int, Int) -> [(Int, Int)]
down a (r, c) = filter (inRange (bounds a)) (reverse [(r, c+y) | y <- [1..100]])

solve1 :: [[Int]] -> Int
solve1 hs = foldr countVisible edges innerIndices
    where a = toArray hs
          ((minX, minY), (maxX, maxY)) = bounds a
          edges = 2 * (maxY+1) + 2 * (maxX+1) - 4
          innerIndices = [(x, y) | x <- [minX+1..maxX-1], y <- [minY+1..maxY-1]]
          countVisible i n = if l || r || u || d then n+1 else n
              where ah = a ! i
                    lineOfSight = map (a !)
                    l = all (< ah) (lineOfSight $ left a i)
                    r = all (< ah) (lineOfSight $ right a i)
                    u = all (< ah) (lineOfSight $ up a i)
                    d = all (< ah) (lineOfSight $ down a i)

solve2 :: [[Int]] -> Int
solve2 hs = maximum ss
    where a = toArray hs
          ((minX, minY), (maxX, maxY)) = bounds a
          innerIndices = [(x, y) | x <- [minX+1..maxX-1], y <- [minY+1..maxY-1]]
          ss = foldr calculateScenicScore [] innerIndices
          calculateScenicScore i ss' = s:ss'
            where s = l * r * u * d
                  ah = a ! i
                  l = fst $ foldr countVisible (0, False) (left a i)
                  r = fst $ foldr countVisible (0, False) (right a i)
                  u = fst $ foldr countVisible (0, False) (up a i)
                  d = fst $ foldr countVisible (0, False) (down a i)
                  countVisible _ (n, True) = (n, True)
                  countVisible t (n, False) = (n+1, a ! t >= ah)
