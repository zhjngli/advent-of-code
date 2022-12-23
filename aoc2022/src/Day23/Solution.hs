module Day23.Solution (solve) where

-- import Debug.Trace
import Data.List ( foldl', groupBy, sort )
import qualified Data.Set as S

solve :: IO ()
solve = do
    input <- readFile "./src/Day23/input.txt"
    putStrLn $ "2022.23.1: " ++ show (solve1 $ lines input)
    putStrLn $ "2022.23.2: " ++ show (solve2 $ lines input)

type Loc = (Int, Int)
type IxMoves = (Loc -> [Loc], (Int, Int))

constructElfMap :: [String] -> S.Set Loc
constructElfMap ls = finalMM
    where (finalMM, _) = foldl' addMMLine (S.empty, (1, 1)) ls
          addMMLine (mmInProgress, (r, maxC)) l = (mmInProgress', (r+1, if c > maxC then c else maxC))
            where (mmInProgress', c) = foldl' addMM (mmInProgress, 1) l
                  addMM (mm, c') '.' = (mm, c'+1)
                  addMM (mm, c') '#' = (S.insert (r, c') mm, c'+1)
                  addMM _ s = error ("invalid elf map char: " ++ [s])

neighborIxs :: Loc -> [Loc]
neighborIxs (r, c) = [(r+dr, c+dc) | (dr, dc) <- [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (1, -1), (-1, 1), (-1, -1)]]

northIxs :: Loc -> [Loc]
northIxs (r, c) = [(r+dr, c+dc) | (dr, dc) <- [(-1, -1), (-1, 0), (-1, 1)]]

southIxs :: Loc -> [Loc]
southIxs (r, c) = [(r+dr, c+dc) | (dr, dc) <- [(1, -1), (1,0), (1, 1)]]

westIxs :: Loc -> [Loc]
westIxs (r, c) = [(r+dr, c+dc) | (dr, dc) <- [(-1, -1), (0, -1), (1, -1)]]

eastIxs :: Loc -> [Loc]
eastIxs (r, c) = [(r+dr, c+dc) | (dr, dc) <- [(-1, 1), (0, 1), (1, 1)]]

propose :: S.Set Loc -> Loc -> [IxMoves] -> Maybe Loc
propose m l@(r, c) ixs =
    if all (`S.notMember` m) (neighborIxs l) then Nothing
    else (
        let f (Just p) _ = Just p
            f Nothing (ix, (dr, dc)) =
                if all (`S.notMember` m) (ix l) then Just (r+dr, c+dc)
                else Nothing
        in foldl' f Nothing ixs
    )

proposals :: S.Set Loc -> [IxMoves] -> [(Loc, Loc)]
proposals m ixs = S.foldr getProposal [] m
    where getProposal l ps =
            case propose m l ixs of
            Nothing -> ps
            Just p -> (p, l):ps

filterProposals :: [(Loc, Loc)] -> [(Loc, Loc)]
filterProposals = concat . filter (\ls -> length ls == 1) . groupBy (\(p1, _) (p2, _) -> p1 == p2) . sort

moveElves :: S.Set Loc -> [(Loc, Loc)] -> S.Set Loc
moveElves = foldr move
    where move (p, l) m = S.insert p (S.delete l m)

elfRound :: S.Set Loc -> [IxMoves] -> S.Set Loc
elfRound m ixs = moveElves m ps
    where ps = filterProposals $ proposals m ixs

elfRoundN :: Int -> S.Set Loc -> [IxMoves] -> S.Set Loc
elfRoundN 0 m _ = m
elfRoundN n m ixs = elfRoundN (n-1) (elfRound m ixs) ixs'
    where ixs' = tail ixs ++ [head ixs]

findBounds :: S.Set Loc -> (Int, Int, Int, Int)
findBounds = S.foldr getMins (1, 1, 1, 1)
    where getMins (r, c) (n, s, e, w) = (
                if r < n then r else n,
                if r > s then r else s,
                if c > e then c else e,
                if c < w then c else w
            )

ixsMoves :: [IxMoves]
ixsMoves = [(northIxs, (-1, 0)), (southIxs, (1, 0)), (westIxs, (0, -1)), (eastIxs, (0, 1))]

solve1 :: [String] -> Int
solve1 ls = (s-n+1) * (e-w+1) - S.size m
    where m = elfRoundN 10 (constructElfMap ls) ixsMoves
          (n, s, e, w) = findBounds m

countElfRounds :: S.Set Loc -> [IxMoves] -> Int -> Int
countElfRounds m ixs n =
    if m == m' then n
    else countElfRounds m' ixs' (n+1)
    where m' = elfRound m ixs
          ixs' = tail ixs ++ [head ixs]

solve2 :: [String] -> Int
solve2 ls = countElfRounds (constructElfMap ls) ixsMoves 1
