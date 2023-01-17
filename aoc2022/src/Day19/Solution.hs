{-# LANGUAGE FlexibleContexts #-}
module Day19.Solution (solve) where

-- import Debug.Trace
import Data.Maybe
import qualified Control.Monad.State.Strict as St
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day19/input.txt"
    putStrLn $ "2022.19.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2022.19.2: " ++ show (solve2 $ parseInput input)

data Resource = Ore | Clay | Obs | Geo deriving (Show, Eq, Ord, Enum)
data Blueprint = B {
    bpid :: !Int,
    oreBot :: !(M.Map Resource Int),
    clayBot :: !(M.Map Resource Int),
    obsBot :: !(M.Map Resource Int),
    geoBot :: !(M.Map Resource Int),
    maxSpend :: !(M.Map Resource Int)
} deriving (Show, Eq, Ord)

parseBlueprints :: CharParser st [Blueprint]
parseBlueprints = endBy1 parseBluePrint newline
    where parseBluePrint = do
            _ <- string "Blueprint "
            bid <- many1 digit
            let b = read bid
            _ <- string ": Each ore robot costs "
            obc <- many1 digit
            let oreBotCost = read obc
            _ <- string " ore. Each clay robot costs "
            cbc <- many1 digit
            let clayBotCost = read cbc
            _ <- string " ore. Each obsidian robot costs "
            oboc <- many1 digit
            let obsBotOreCost = read oboc
            _ <- string " ore and "
            obcc <- many1 digit
            let obsBotClayCost = read obcc
            _ <- string " clay. Each geode robot costs "
            gborec <- many1 digit
            let geoBotOreCost = read gborec
            _ <- string " ore and "
            gbobsc <- many1 digit
            let geoBotObsCost = read gbobsc
            _ <- string " obsidian."
            let mOre = maximum [oreBotCost, clayBotCost, obsBotOreCost, geoBotOreCost]
            let mClay = maximum [obsBotClayCost]
            let mObs = maximum [geoBotObsCost]
            return B {
                    bpid = b,
                    oreBot = M.fromList [(Ore, oreBotCost)],
                    clayBot = M.fromList [(Ore, clayBotCost)],
                    obsBot = M.fromList [(Ore, obsBotOreCost), (Clay, obsBotClayCost)],
                    geoBot = M.fromList [(Ore, geoBotOreCost), (Obs, geoBotObsCost)],
                    maxSpend = M.fromList $ zip [Ore, Clay, Obs] [mOre, mClay, mObs]
                }

parseInput :: String -> [Blueprint]
parseInput i = case parse parseBlueprints "" i of
    Left e -> error (show e)
    Right bs -> bs

type Bots = M.Map Resource Int
type Resources = M.Map Resource Int

maxGeodes :: Blueprint -> Int -> Bots -> Resources -> St.State Int Int
maxGeodes _ 0 _ resources = return $ resources M.! Geo
maxGeodes bp remainingTime bots resources = do
    best <- St.get
    let noopMax = resources M.! Geo + bots M.! Geo * remainingTime
    let upperBound = noopMax + (remainingTime * (remainingTime - 1) `div` 2)
    if best >= upperBound then return 0
    else do
        let build (bot, cost)
                | bot /= Geo && ((bots M.! bot) >= (maxSpend bp M.! bot)) = Nothing
                | otherwise =
                    case M.foldrWithKey buildTime (Just 0) cost of
                    Nothing -> Nothing
                    Just t -> if t+1 > remainingTime then Nothing else Just (bot, cost, t+1)
                    where buildTime r n t =
                            let rbots = bots M.! r in
                            if rbots == 0 then Nothing
                            else let (dt, rt) = (n - resources M.! r) `divMod` rbots in
                                if rt == 0 then max dt <$> t
                                else max (dt + 1) <$> t
        let builds = mapMaybe build $ zip [Ore, Clay, Obs, Geo] [oreBot bp, clayBot bp, obsBot bp, geoBot bp]
        tryBuilds <- mapM (\(bot, cost, dt) ->
                let bots' = M.adjust (+1) bot bots
                    newResources = foldr (\r res -> M.insert r (resources M.! r + dt * bots M.! r) res) M.empty [Ore, Clay, Obs, Geo]
                    resourcesMinusCost = M.foldrWithKey (\r n res -> M.adjust (\x -> x-n) r res) newResources cost
                in maxGeodes bp (remainingTime - dt) bots' resourcesMinusCost
                ) builds

        let maxGeode = maximum $ noopMax:tryBuilds
        St.modify (max maxGeode)
        return maxGeode

solve1 :: [Blueprint] -> Int
solve1 bps = sum $ map (\bp -> bpid bp * mg bp) bps
    where mg bp = St.evalState (maxGeodes bp 24 bots resources) 0
          bots = M.fromList [(Ore, 1), (Clay, 0), (Obs, 0), (Geo, 0)]
          resources = M.fromList $ zip [Ore, Clay, Obs, Geo] (repeat 0)

solve2 :: [Blueprint] -> Int
solve2 bps = product $ map mg (take 3 bps)
    where mg bp = St.evalState (maxGeodes bp 32 bots resources) 0
          bots = M.fromList [(Ore, 1), (Clay, 0), (Obs, 0), (Geo, 0)]
          resources = M.fromList $ zip [Ore, Clay, Obs, Geo] (repeat 0)
