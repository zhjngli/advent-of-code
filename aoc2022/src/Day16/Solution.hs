module Day16.Solution (solve) where

import Lib.Common

-- import Debug.Trace
import Data.List ( minimumBy, tails )
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day16/input.txt"
    test <- readFile "./src/Day16/test.txt"
    putStrLn $ "2022.16.2 test: " ++ show (solve2 $ parseInput test)
    -- putStrLn $ "2022.16.2: " ++ show (solve2 $ parseInput input)
    putStrLn $ "2022.16.1 test: " ++ show (solve1 $ parseInput test)
    putStrLn $ "2022.16.1: " ++ show (solve1 $ parseInput input)

type ValveId = String
type Valve = (Int, [ValveId])
type System = M.Map ValveId Valve

parseValve :: CharParser st (ValveId, Valve)
parseValve = do
    _ <- string "Valve "
    vid <- many1 upper
    _ <- string " has flow rate="
    r <- many1 digit
    _ <- try (string "; tunnels lead to valves ") <|> string "; tunnel leads to valve "
    vs <- sepBy1 (many1 upper) (string ", ")
    return (vid, (read r, vs))

parseSystem :: CharParser st System
parseSystem = do
    vs <- endBy1 parseValve newline
    return $ M.fromList vs

parseInput :: String -> System
parseInput i = case parse parseSystem "" i of
    Left e -> error (show e)
    Right s -> s

-- current state: system, where you are, time, time limit, open valves
type SystemState = (System, ValveId, Int, Int, S.Set Valve)

-- find fastest path between all pairs of valves that have non zero flow rate and initial valve
fastestPathBetweenValves :: System -> ValveId -> M.Map ValveId [(ValveId, Int)]
fastestPathBetweenValves s initValve = foldr (\((x, y), dt) m -> MS.insertWith (++) x [(y, dt)] $ MS.insertWith (++) y [(x, dt)] m) M.empty flowValvePairs
    where flowValves = initValve:(map fst . M.toList $ M.filter (\(r, _) -> r /= 0) s)
          flowValvePairs = [
                ((x, y), t)
                | (x : ys) <- tails flowValves
                , let ts = dijkstras [x] (\ _ _ -> C 1) ns
                , y <- ys
                , let t = case ts M.! y of
                            C c -> c
                            Inf -> error "infinite cost"
            ]
          ns v = snd $ s M.! v

-- find fastest path to closed valves that have nonzero flow rate from a state
fastestPathToClosedValves :: M.Map ValveId [(ValveId, Int)] -> SystemState -> [SystemState]
fastestPathToClosedValves dts (sys, vid, t, limit, openValves) = [
        (sys, y, finalT, limit, openValves)
        | (y, dt) <- dts M.! vid
        , let valve@(r, _) = sys M.! y
        , r /= 0
        , S.notMember valve openValves
        , let finalT = t + dt
        , finalT < limit
    ]

neighbors :: M.Map ValveId [(ValveId, Int)] -> SystemState -> [SystemState]
neighbors dts ss@(s, _, t, limit, vs)
    | t >= limit = []
    | M.filter (\v@(r, _) -> S.notMember v vs && r /= 0) s == M.empty = [] -- no closed valves, don't need to do anything
    | otherwise = openNewValves
    where openNewValves = [
                (s, vid', t'+1, limit, S.insert valve vs')
                | let newLocations = fastestPathToClosedValves dts ss
                , (_, vid', t', _, vs') <- newLocations
                , let valve@(rate, _) = s M.! vid'
                , rate /= 0
                , S.notMember valve vs'
                , t'+1 < limit
            ]

cost :: SystemState -> SystemState -> Cost Int
cost (s, _, t1, _, openVs) (_, _, t2, _, _) = C notReleased
    where notReleased = (* (t2 - t1)) . sum $ M.map fst closedValves
          closedValves = M.filter (`S.notMember` openVs) s

solve1 :: System -> Int
solve1 s = -- trace (show $ M.size allStates)
    fullPotentialFlowOverTimeLimit - minMissedFlow
    where limit = 30
          initValve = "AA"
          start = (s, initValve, 0, limit, S.empty)
          precalculatedPaths = fastestPathBetweenValves s initValve
          ns = neighbors precalculatedPaths
          flowValves = S.fromList . map snd . M.toList $ M.filter (\(r, _) -> r /= 0) s
          fullPotentialFlowOverTimeLimit = limit * (sum . map fst $ S.toList flowValves)
          allStates = dijkstras [start] cost ns
          nonNullFlowStates = M.filterWithKey (\(_, _, _, _, vs) _ -> not $ S.null vs) allStates
          endStates = M.filterWithKey (\state _ -> null $ ns state) nonNullFlowStates
          missedFlow ((_, _, t, lim, vs), C c) = c + (lim - t) * sum (M.map fst $ M.filter (`S.notMember` vs) s)
          missedFlow (_, Inf) = error "infinite cost"
          minMissedFlow = minimum . map missedFlow $ M.toList endStates

-- current state: system, where you are, where elephant is, your time, elephant time, time limit, your open valves, elephant open valves
type SystemState2 = (System, ValveId, ValveId, Int, Int, Int, S.Set Valve, S.Set Valve)

neighbors2 :: M.Map ValveId [(ValveId, Int)] -> SystemState2 -> [SystemState2]
neighbors2 dts (s, yid, eid, yt, et, limit, yvs, evs)
    | M.filter (\v@(r, _) -> S.notMember v vs && r /= 0) s == M.empty = [] -- no closed valves, don't need to do anything
    | yt >= limit || et >= limit = []
    | (S.fromList . map snd . M.toList $ M.filter (\(r, _) -> r /= 0) s) == vs = []
    | otherwise = openNewValves
    where vs = S.union yvs evs
          openNewValves = [
                (s, yid', eid', yt'+1, et'+1, limit, S.insert y yvs, S.insert e evs)
                | (_, yid', yt', _, _) <- fastestPathToClosedValves dts (s, yid, yt, limit, vs)
                , (_, eid', et', _, _) <- fastestPathToClosedValves dts (s, eid, et, limit, vs)
                , yid' /= eid' -- don't let actors go to the same location
                , let y@(yr, _) = s M.! yid'
                , let e@(er, _) = s M.! eid'
                , yr /= 0
                , er /= 0
                , S.notMember y vs
                , S.notMember e vs
                , yt'+1 < limit
                , et'+1 < limit
            ]

-- counts cost of both actors
cost2 :: SystemState2 -> SystemState2 -> Cost Int
cost2 (s1, yid1, eid1, yt1, et1, limit1, yvs1, evs1) (s2, yid2, eid2, yt2, et2, limit2, yvs2, evs2) = addCost ycost ecost
    where ycost = cost (s1, yid1, yt1, limit1, yvs1) (s2, yid2, yt2, limit2, yvs2)
          ecost = cost (s1, eid1, et1, limit1, evs1) (s2, eid2, et2, limit2, evs2)

solve2 :: System -> Int
solve2 s = -- trace (show minMissedFlow)
    2 * fullPotentialFlowOverTimeLimit - fst minMissedFlow + (fullLimit - limit) * fullFlow
    where fullLimit = 26
          limit = 26
          initValve = "AA"
          start = (s, initValve, initValve, 0, 0, limit, S.empty, S.empty)
          precalculatedPaths = fastestPathBetweenValves s initValve
          ns = -- trace (M.foldrWithKey (\k a s -> s ++ "\n" ++ show k ++ ": " ++ show (sortBy (\a b -> compare (snd a) (snd b)) a)) "" precalculatedPaths)
            neighbors2 precalculatedPaths
          flowValves = S.fromList . map snd . M.toList $ M.filter (\(r, _) -> r /= 0) s
          fullFlow = sum . map fst $ S.toList flowValves
          fullPotentialFlowOverTimeLimit = limit * fullFlow
          allStates = dijkstras [start] cost2 ns
          nonNullFlowStates = M.filterWithKey (\(_, _, _, _, _, _, yvs, evs) _ -> not (S.null yvs || S.null evs)) allStates
          endStates = M.filterWithKey (\state _ -> null $ ns state) nonNullFlowStates
          missedFlow ((_, _, _, yt, et, lim, yvs, evs), C c) = c
            + (lim - yt) * sum (M.map fst $ M.filter (`S.notMember` yvs) s)
            + (lim - et) * sum (M.map fst $ M.filter (`S.notMember` evs) s)
          missedFlow (_, Inf) = error "infinite cost"
          minMissedFlow = minimumBy (\a b -> compare (fst a) (fst b)) . map (\st@((_, _, _, _, _, _, yvs, evs), _) -> (missedFlow st, S.union yvs evs == flowValves)) $ M.toList endStates
