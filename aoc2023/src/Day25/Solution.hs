module Day25.Solution (solve) where

import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Foldable
import System.Random

solve :: IO ()
solve = do
    input <- readFile "./src/Day25/input.txt"
    r <- getStdGen
    putStrLn $ "2023.25: " ++ show (solve' r $ parseInput input)

type Edges = S.Set (String, String)
type AdjList = M.Map String (S.Set String)

parseInput :: String -> (Edges, AdjList)
parseInput i = foldl'
    (\(es, g) l ->
        let (from, tos) = ftSplit l in
        foldl' (\(es', g') t ->
            (
                S.insert (from, t) es',
                M.insertWith S.union from (S.singleton t)
                (M.insertWith S.union t (S.singleton from) g')
            )
        )
        (es, g) tos
    )
    (S.empty, M.empty) (lines i)
    where
        ftSplit l = case splitOn ": " l of
            [from, to] -> (from, toSplit to)
            _ -> error ("can't split line: " ++ l)
        toSplit t = let tos = splitOn " " t in
            case length tos of
            0 -> error ("can't read tos: " ++ t)
            _ -> tos

randElem :: (RandomGen r) => S.Set a -> r -> (a, r)
randElem s r = (S.elemAt n s, r')
    where (n, r') = randomR (0, S.size s - 1) r

vertices :: AdjList -> S.Set String
vertices = M.foldlWithKey' (\vs v us -> S.insert v $ S.union vs us) S.empty

-- helper function which finds the partitions that two vertices, u and v, belong to
-- if they are in the same partition, that partition is returned twice, along with the rest of the partitions
-- otherwise, return both partitions that those vertices are in, and the rest of the partitions
findPartition :: (Foldable t, Ord a) => a -> a -> t (S.Set a) -> (S.Set a, S.Set a, [S.Set a])
findPartition u v ps
    | S.member v uPart = (uPart, uPart, psSansU)
    | otherwise = (uPart, vPart, psSansV)
    where
        helper n = foldl' (\(nPart, rest) p -> if S.member n p then (S.union p nPart, rest) else (nPart, p:rest)) (S.empty, [])
        (uPart, psSansU) = helper u ps
        (vPart, psSansV) = helper v psSansU

kargers :: (RandomGen r) => r -> (Edges, AdjList) -> (r, Edges, [S.Set String])
kargers r (es, g) = (r', edgesToRemove, ps)
    where
        vs = vertices g
        numVertices = S.size vs
        partitions = foldl' (\acc v -> S.singleton v : acc) [] vs
        (r', ps) = cutGraph r numVertices (es, g) partitions
        edgesToRemove = foldl'
            (\acc (u, v) ->
                let (uPart, vPart, _) = findPartition u v ps in
                if uPart /= vPart then S.insert (u, v) acc else acc
            )
            S.empty es

cutGraph :: (RandomGen r) => r -> Int -> (Edges, AdjList) -> [S.Set String] -> (r, [S.Set String])
cutGraph r vs (es, g) partitions
    | vs <= 2 = (r', partitions)
    -- u and v not in the same partition, combine them
    | uPart /= vPart = cutGraph r' (vs-1) (es, g) partitions'
    | otherwise = cutGraph r' vs (es, g) partitions
    where
        ((u, v), r') = randElem es r
        (uPart, vPart, partitionsSansUV) = findPartition u v partitions
        partitions' = S.union vPart uPart : partitionsSansUV

solve' :: (RandomGen r) => r -> (Edges, AdjList) -> Int
solve' r g =
    case S.size es of
    3 -> length p1 * length p2
    _ -> solve' r' g
    where
        (r', es, ps) = kargers r g
        (p1, p2) = case ps of
            [x, y] -> (x, y)
            _ -> error ("kargers failed to split into two partitions: " ++ show ps)
