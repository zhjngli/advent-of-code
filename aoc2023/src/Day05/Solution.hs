module Day05.Solution (solve) where

import Data.List (foldl')
import Text.ParserCombinators.Parsec

solve :: IO ()
solve = do
    input <- readFile "./src/Day05/input.txt"
    putStrLn $ "2023.05.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.05.2: " ++ show (solve2 $ parseInput input)

type Seeds = [Int]
type SeedSoilMap = Int -> Int
type SoilFertilizerMap = Int -> Int
type FertilizerWaterMap = Int -> Int
type WaterLightMap = Int -> Int
type LightTempMap = Int -> Int
type TempHumidityMap = Int -> Int
type HumidityLocationMap = Int -> Int

parseMapLine :: CharParser st (Int, Int, Int)
parseMapLine = do
    x <- many1 digit
    _ <- char ' '
    y <- many1 digit
    _ <- char ' '
    r <- many1 digit
    return (read x, read y, read r)

toMap :: [(Int, Int, Int)] -> Int -> Int
toMap ranges i = fst f
    where f = foldl'
            (\(i', found) (dest, src, r) ->
                -- if already found mapping, don't do anything else
                if found then (i', True)
                -- map domain to range
                else if i' >= src && i' < src + r then (i' - src + dest, True)
                -- if i doesn't match any of the domains, output is the same
                else (i', False))
            (i, False)
            ranges

parseI :: CharParser st (Seeds, SeedSoilMap, SoilFertilizerMap, FertilizerWaterMap, WaterLightMap, LightTempMap, TempHumidityMap, HumidityLocationMap)
parseI = do
    _ <- string "seeds: "
    seeds <- sepBy1 (many1 digit) (char ' ')
    _ <- newline
    _ <- newline
    _ <- string "seed-to-soil map:"
    _ <- newline
    ssm <- endBy1 parseMapLine newline
    _ <- newline
    _ <- string "soil-to-fertilizer map:"
    _ <- newline
    sfm <- endBy1 parseMapLine newline
    _ <- newline
    _ <- string "fertilizer-to-water map:"
    _ <- newline
    fwm <- endBy1 parseMapLine newline
    _ <- newline
    _ <- string "water-to-light map:"
    _ <- newline
    wlm <- endBy1 parseMapLine newline
    _ <- newline
    _ <- string "light-to-temperature map:"
    _ <- newline
    ltm <- endBy1 parseMapLine newline
    _ <- newline
    _ <- string "temperature-to-humidity map:"
    _ <- newline
    thm <- endBy1 parseMapLine newline
    _ <- newline
    _ <- string "humidity-to-location map:"
    _ <- newline
    hlm <- endBy1 parseMapLine newline
    return (map read seeds, toMap ssm, toMap sfm, toMap fwm, toMap wlm, toMap ltm, toMap thm, toMap hlm)

parseInput :: String -> (Seeds, SeedSoilMap, SoilFertilizerMap, FertilizerWaterMap, WaterLightMap, LightTempMap, TempHumidityMap, HumidityLocationMap)
parseInput i = case parse parseI "" i of
    Left e -> error (show e)
    Right cs -> cs

solve1 :: (Seeds, SeedSoilMap, SoilFertilizerMap, FertilizerWaterMap, WaterLightMap, LightTempMap, TempHumidityMap, HumidityLocationMap) -> Int
solve1 (ss, ssm, sfm, fwm, wlm, ltm, thm, hlm) = minimum $ map (hlm . thm . ltm . wlm . fwm . sfm . ssm) ss

solve2 :: (Seeds, SeedSoilMap, SoilFertilizerMap, FertilizerWaterMap, WaterLightMap, LightTempMap, TempHumidityMap, HumidityLocationMap) -> Int
solve2 (ss, ssm, sfm, fwm, wlm, ltm, thm, hlm) = solve1 (newSeeds ss, ssm, sfm, fwm, wlm, ltm, thm, hlm)
    where newSeeds (seedStart:seedRange:ss') = [seedStart..seedStart+seedRange-1] ++ newSeeds ss'
          newSeeds [] = []
          newSeeds _ = error "odd number of seeds"
