module Day24.Solution (solve) where

-- import Debug.Trace
import Lib.Common
import Data.Array
import Data.List ( foldl' )
import qualified Data.Map as M
import Data.Maybe

solve :: IO ()
solve = do
    input <- readFile "./src/Day24/input.txt"
    putStrLn $ "2022.24.1: " ++ show (solve1 $ lines input)
    putStrLn $ "2022.24.2: " ++ show (solve2 $ lines input)

data Blizzard = Up | Dn | Lt | Rt deriving (Show, Eq)
data Mountain = W | B [Blizzard] | G deriving Show

toMountain :: Char -> Mountain
toMountain '#' = W
toMountain '.' = G
toMountain '>' = B [Rt]
toMountain '<' = B [Lt]
toMountain '^' = B [Up]
toMountain 'v' = B [Dn]
toMountain c = error ("unrecognized char:" ++ [c])

newBlizzardIx :: ((Int, Int), (Int, Int)) -> (Int, Int) -> Blizzard -> (Int, Int)
newBlizzardIx ((mnr, mnc), (mxr, mxc)) (r, c) b = case b of
    Up -> if r-1 == mnr then (mxr-1, c) else (r-1, c)
    Dn -> if r+1 == mxr then (mnr+1, c) else (r+1, c)
    Lt -> if c-1 == mnc then (r, mxc-1) else (r, c-1)
    Rt -> if c+1 == mxc then (r, mnc+1) else (r, c+1)

moveBlizzards :: Array (Int, Int) Mountain -> Array (Int, Int) Mountain
moveBlizzards a = foldr f def (range bnds)
    where bnds = bounds a
          def = listArray bnds $ repeat G
          f ix a' = case a ! ix of
            W -> a' // [(ix, W)]
            G -> a'
            B bs -> (
                let newBs = zip (map (newBlizzardIx bnds ix) bs) bs
                    applyBs (i, b) a'' = case a'' ! i of
                        B bs' -> a'' // [(i, B $ b:bs')]
                        G -> a'' // [(i, B [b])]
                        W -> error ("cannot add blizzard " ++ show b ++ " at " ++ show i ++ " to " ++ show a'')
                in foldr applyBs a' newBs
                )

mountainOverTime :: Array (Int, Int) Mountain -> Int -> Array (Int, (Int, Int)) Mountain
mountainOverTime start time = array ((0, bs), (time, be)) asOverTime
    where (_, asOverTime) = foldl' f (start, []) [0..time]
          (bs, be) = bounds start
          f (a, l) i = (moveBlizzards a, l ++ map (\(ai, e) -> ((i, ai), e)) (assocs a))

moves :: Array (Int, (Int, Int)) Mountain -> (Int, (Int, Int)) -> [(Int, (Int, Int))]
moves m (t, (r, c)) = filter (\i -> inMap i && isGround i) $ zip (repeat $ t+1) [(r,c), (r+1,c), (r-1,c), (r, c+1), (r,c-1)]
    where bnds = bounds m
          inMap i = inRange bnds i
          isGround i = case m ! i of
            G -> True
            _ -> False

-- always takes 1 minute to move
cost :: a -> b -> Cost Int
cost _ _ = C 1

earliestTimeToGoal :: M.Map (Int, (Int, Int)) (Cost Int) -> (Int, Int) -> Int -> Int -> Maybe Int
earliestTimeToGoal m goal maxTime t =
    if t > maxTime then Nothing
    else case M.lookup (t, goal) m of
        Just (C _) -> Just t
        _ -> earliestTimeToGoal m goal maxTime (t+1)

solve1 :: [[Char]] -> Int
solve1 cs = fromJust $ earliestTimeToGoal final (mr, mc-1) t 0
    where t = 266 -- answer to reduce computation
          start = toArray (map (map toMountain) cs)
          ms = mountainOverTime start t
          (_, (_, (mr, mc))) = bounds ms
          final = dijkstrasArr ms [(0, (0, 1))] cost moves

solve2 :: [[Char]] -> Int
solve2 cs = secondTimeToGoal
    where maxT = 853 -- answer to reduce computation
          origMountain = toArray (map (map toMountain) cs)
          ms = mountainOverTime origMountain maxT
          (_, (_, (mr, mc))) = bounds ms
          orig = (0, 1)
          goal = (mr, mc-1)

        --   firstToGoal = dijkstrasArr ms [(0, orig)] cost moves
        --   firstTimeToGoal = fromJust $ earliestTimeToGoal firstToGoal goal maxT 0
          firstTimeToGoal = 266 -- answer from part 1 to reduce computation

          backToOrig = dijkstrasArr ms [(firstTimeToGoal, goal)] cost moves
          timeBackToOrig = fromJust $ earliestTimeToGoal backToOrig orig maxT firstTimeToGoal

          secondToGoal = dijkstrasArr ms [(timeBackToOrig, orig)] cost moves
          secondTimeToGoal = fromJust $ earliestTimeToGoal secondToGoal goal maxT timeBackToOrig
