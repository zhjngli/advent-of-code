module Day22.Solution (solve) where

import Data.Array
import Data.List ( sortBy, minimumBy, nub, foldl' )
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map.Strict as M

solve :: IO ()
solve = do
    input <- readFile "./src/Day22/input.txt"
    putStrLn $ "2023.22.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.22.2: " ++ show (solve2 $ parseInput input)

parseInput :: String -> [S.Set (Int, Int, Int)]
parseInput = map S.fromList . sortBy minZ . map (allBricks . brickEnds) . lines
    where
        brickEnds = splitOn "~"
        allBricks be = case be of
            [s, e] -> case (splitOn "," s, splitOn "," e) of
                ([x1,y1,z1], [x2,y2,z2]) -> range ((read x1, read y1, read z1), (read x2, read y2, read z2))
                _ -> error ("cannot parse brick start or end: " ++ show s ++ ", " ++ show e)
            _ -> error ("cannot parse brick ends: " ++ show be)

minZ :: (Foldable t) => t (Int, Int, Int) -> t (Int, Int, Int) -> Ordering
minZ b1 b2 =
    let brickMinZ = minimumBy (\(_, _, bz1) (_, _, bz2) -> compare bz1 bz2)
        (_, _, z1) = brickMinZ b1
        (_, _, z2) = brickMinZ b2
    in compare z1 z2

lower :: S.Set (Int, Int, Int) -> S.Set (Int, Int, Int)
lower = S.map (\(x, y, z) -> (x, y, z-1))

belowGround :: S.Set (Int, Int, Int) -> Bool
belowGround = any (\(_, _, z) -> z < 1)

-- assumes bricks are sorted by z height
-- returns settled bricks, map of brick id to its dependencies, and map of brick id to ids on which it depends
-- starts brick id from 1 and counts up
fall :: [S.Set (Int, Int, Int)] -> ([S.Set (Int, Int, Int)], M.Map Int (S.Set Int), M.Map Int (S.Set Int))
fall = fall' [] 1 M.empty M.empty M.empty
    where
        fall' settledBs _ _ depsOf depsOn [] = (settledBs, depsOf, depsOn)
        fall' settledBs i ids depsOf depsOn (b:bs) = fall' (b':settledBs) (i+1) ids' depsOf' depsOn' bs
            where
                (b', ids', depsOf', depsOn') = fallB b
                settled = S.unions settledBs
                fallB brick =
                    let lb = lower brick
                        newIds = S.foldl' (\m block -> M.insert block i m) ids brick
                    in
                    if belowGround lb then (brick, newIds, depsOf, depsOn)
                    else if not (S.disjoint settled lb) then
                        let dep = S.intersection settled lb
                            depIds = nub $ S.foldl' (\acc block -> ids M.! block : acc) [] dep
                            depsOn'' = foldl' (\acc depId -> M.insertWith S.union i (S.singleton depId) acc) depsOn depIds
                            depsOf'' = foldl' (\acc depId -> M.insertWith S.union depId (S.singleton i) acc) depsOf depIds
                        in
                        (brick, newIds, depsOf'', depsOn'')
                    else fallB lb

-- canDisintegrateBrute :: [S.Set (Int, Int, Int)] -> Int
-- canDisintegrateBrute bs = length disintegrated
--     where
--         sortedBs = map S.fromList . sortBy minZ . map S.toList $ bs
--         sortedBsSet = S.fromList sortedBs
--         allOneBrickRemoved = map ((map S.fromList . sortBy minZ . map S.toList) . S.toList) $ S.foldl' (\acc s -> S.delete s sortedBsSet : acc) [] sortedBsSet
--         disintegrated = filter
--             (\maybeSettled ->
--                 let (actuallySettled, _, _) = fall maybeSettled in
--                 S.null $ S.difference (S.unions actuallySettled) (S.unions maybeSettled)
--             )
--             allOneBrickRemoved

canDisintegrate :: M.Map Int (S.Set Int) -> Int -> Int
canDisintegrate deps n = n - S.size (supportingBlocks deps)

-- blocks where it is the only block supporting some other blocks
supportingBlocks :: M.Map k (S.Set Int) -> S.Set Int
supportingBlocks = M.foldl' (\x s -> if S.size s == 1 then S.union x s else x) S.empty

solve1 :: [S.Set (Int, Int, Int)] -> Int
solve1 bs = canDisintegrate depsOn (length bs)
    where
        (_, _, depsOn) = fall bs

solve2 :: [S.Set (Int, Int, Int)] -> Int
solve2 bs = sum $ map (\u -> chain S.empty [u]) [1..length bs]
    where
        (_, depsOf, depsOn) = fall bs
        chain disintegratedSet [] = S.size disintegratedSet - 1 -- subtract the initial one we disintegrated
        chain disintegratedSet (b:blocksToProcess) = chain disintegratedSet' (blocksToProcess ++ newToProcess)
            where
                disintegratedSet' = S.insert b disintegratedSet
                newToProcess =
                    if M.member b depsOf then
                        S.foldl'
                        (\acc depOfB ->
                            if M.member depOfB depsOn then
                                let deps = depsOn M.! depOfB in
                                -- if i is not already disintegrated, and if we've disintegrated all of i's dependencies
                                if S.notMember depOfB disintegratedSet' && S.isSubsetOf deps disintegratedSet' then depOfB:acc
                                else acc
                            else acc
                        ) [] (depsOf M.! b)
                    else []
