module Day16.Solution (solve) where

import Lib.Common

-- import Debug.Trace
import Data.List ( tails )
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day16/input.txt"
    putStrLn $ "2022.16.1: " ++ show (solve1 $ parseInput input)
    -- putStrLn $ "2022.16.2: " ++ show (solve2 $ parseInput input)

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
neighbors dts ss@(s, vid, t, limit, vs)
    | t >= limit = []
    | M.filter (\v@(r, _) -> S.notMember v vs && r /= 0) s == M.empty = [] -- no closed valves, don't need to do anything
    | otherwise = if not $ null openCurrentValve then openCurrentValve else newLocations
    where valve@(rate, _) = s M.! vid
          openCurrentValve = [(s, vid, t+1, limit, S.insert valve vs) | rate /= 0, S.notMember valve vs, t+1 < limit]
          newLocations = fastestPathToClosedValves dts ss

cost :: SystemState -> SystemState -> Cost Int
cost (s, _, t1, _, openVs1) (_, _, t2, _, _) = C notReleased
    where notReleased = (* (t2 - t1)) . sum $ M.map fst closedValves
          closedValves = M.filter (`S.notMember` openVs1) s

solve1 :: System -> Int
solve1 s = fullPotentialFlowOverTimeLimit - minMissedFlow
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
