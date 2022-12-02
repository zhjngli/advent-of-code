module Y2021.Day23.Solution where

import Data.List
import Data.Array
import qualified Data.Map as M
import qualified Data.Set as S

data Space = A | B | C | D | N deriving (Show, Ord, Eq)
type HallI = Int
type RoomI = Int
type Hall = Array HallI Space
type Room = Array RoomI Space
type Rooms = Array Int Room
data Area = Area { h :: Hall,
                   r :: Rooms
                 } deriving (Ord, Eq)

showHall :: Hall -> String
showHall h = show (h ! 0) ++ show (h ! 1) ++ "." ++ show (h ! 2) ++ "." ++ show (h ! 3) ++ "." ++ show (h ! 4) ++ "." ++ show (h ! 5) ++ show (h ! 6)

showRooms :: Rooms -> String
showRooms r = foldl' f [] [x..y]
    where (x, y) = bounds (r ! 0)
          f s i = s ++ "  " ++ show ((r ! 0) ! i) ++ " " ++ show ((r ! 1) ! i) ++ " " ++ show ((r ! 2) ! i) ++ " " ++ show ((r ! 3) ! i) ++ "  \n"

instance Show Area where
    show (Area h r) = showHall h ++ "\n" ++ showRooms r

emptyHall :: Hall
emptyHall = array (0, 6) (zip [0..6] $ repeat N)

fromList :: [a] -> Array Int a
fromList a = let l = length a in listArray (0, l-1) a

input1 :: Area
input1 = Area emptyHall $ fromList $ map fromList [[D, C], [B, C], [B, D], [A, A]]

input2 :: Area
input2 = Area emptyHall $ fromList $ map fromList [[D, D, D, C], [B, C, B, C], [B, B, A, D], [A, A, C, A]]

final1 :: Area
final1 = Area emptyHall $ fromList $ map fromList [[A, A], [B, B], [C, C], [D, D]]

final2 :: Area
final2 = Area emptyHall $ fromList $ map fromList [[A, A, A, A], [B, B, B, B], [C, C, C, C], [D, D, D, D]]

testInput1 :: Area
testInput1 = Area emptyHall $ fromList $ map fromList [[B, A], [C, D], [B, C], [D, A]]

testInput2 :: Area
testInput2 = Area emptyHall $ fromList $ map fromList [[B, D, D, A], [C, C, B, D], [B, B, A, C], [D, A, C, A]]

solve :: IO ()
solve = do
    putStrLn $ "2021.23.1: " ++ show (solve' input1 final1)
    putStrLn $ "2021.23.2: " ++ show (solve' input2 final2)

{-
hall: 01x2x3x4x56
room:   0 1 2 3
-}

roomsInBetween :: HallI -> Int -> Int
roomsInBetween 0 r = r + 1
roomsInBetween 1 r = r + 1
roomsInBetween 2 r = max 1 r
roomsInBetween 3 0 = 2
roomsInBetween 3 1 = 1
roomsInBetween 3 2 = 1
roomsInBetween 3 3 = 2
roomsInBetween 4 r = max 1 (3 - r)
roomsInBetween 5 r = 4 - r
roomsInBetween 6 r = 4 - r
roomsInBetween _ _ = error "unsupported hall index or room number"

distance :: HallI -> Int -> Int
distance hi r
    | hi <= r+1 = r + 1 - hi + roomsInBetween hi r
    | hi >= r+2 = hi - r - 2 + roomsInBetween hi r
    | otherwise = error "don't know what to do"

pathFromRoomToHallI :: Hall -> Int -> HallI -> Bool
pathFromRoomToHallI h r hi
    | hi <= r+1 = all ((== N) . (h !)) [hi..r+1] -- hi left of room
    | hi >= r+2 = all ((== N) . (h !)) [r+2..hi] -- hi right of room
    | otherwise = error "don't know what to do with this input"

pathFromHallIToRoom :: Hall -> HallI -> Int -> Bool
pathFromHallIToRoom h hi r
    | hi <= r+1 = all ((== N) . (h !)) [hi+1..r+1] -- hi left of room
    | hi >= r+2 = all ((== N) . (h !)) [r+2..hi-1] -- hi right of room
    | otherwise = error "don't know what to do with this input"

canEnterRoom :: Room -> Space -> Bool
canEnterRoom r s = all ((\x -> x == N || x == s) . (r !)) [x..y]
    where (x, y) = bounds r

roomComplete :: Room -> Int -> Bool
roomComplete r ri = all ((== getAmphForRoom ri) . (r !)) [x..y]
    where (x, y) = bounds r

moveOutOfRoom :: Room -> (Int, Space, Room)
moveOutOfRoom r = foldl' f (0, N, r) [x..y]
    where (x, y) = bounds r
          f (_, N, r') i = (i, r ! i, r // [(i, N)])
          f b _ = b

moveIntoRoom :: Room -> Space -> (Int, Room)
moveIntoRoom r s = foldr f (-1, r) [x..y]
    where (x, y) = bounds r
          f i (-1, _) = if r ! i == N then (i, r // [(i, s)]) else (-1, r)
          f _ b = b

getRoomForAmph :: Space -> Int
getRoomForAmph A = 0
getRoomForAmph B = 1
getRoomForAmph C = 2
getRoomForAmph D = 3
getRoomForAmph _ = error "no room exists for this amphipod"

getAmphForRoom :: Int -> Space
getAmphForRoom 0 = A
getAmphForRoom 1 = B
getAmphForRoom 2 = C
getAmphForRoom 3 = D
getAmphForRoom _ = error "no amphipod exists for this room"

moveCost :: Space -> Int
moveCost A = 1
moveCost B = 10
moveCost C = 100
moveCost D = 1000
moveCost N = 0

neighborsToHall :: Area -> [(Area, Int)]
neighborsToHall (Area h rs) =
    [(Area h' rs', cost)
    | ri <- [0..3]
    , let r = rs ! ri
    , not $ roomComplete r ri
    , let (i, amph, r') = moveOutOfRoom r
    , amph /= N
    , let rs' = rs // [(ri, r')]
    , let his = filter (pathFromRoomToHallI h ri) [0..6]
    , (h', cost) <- [(h // [(hi, amph)], moveCost amph * (distance hi ri + i + 1)) | hi <- his]
    ]

neighborsToRoom :: Area -> [(Area, Int)]
neighborsToRoom (Area h rs) =
    [(Area h' rs', cost)
    | hi <- [0..6]
    , let ha = h ! hi
    , ha /= N
    , let ri = getRoomForAmph ha
    , pathFromHallIToRoom h hi ri
    , canEnterRoom (rs ! ri) ha
    , let (i, r) = moveIntoRoom (rs ! ri) ha
    , let h' = h // [(hi, N)]
    , let rs' = rs // [(ri, r)]
    , let cost = moveCost ha * (distance hi ri + i + 1)
    ]

neighbors :: Area -> [(Area, Int)]
neighbors a = neighborsToRoom a ++ neighborsToHall a

dijkstras :: (Ord a, Eq a, Ord d, Num d) => (a -> [(a, d)]) -> M.Map a d -> M.Map a a -> S.Set (a, d) -> (M.Map a d, M.Map a a)
dijkstras neighbors dist pred q
    | S.null q = (dist, pred)
    | otherwise = dijkstras neighbors dist' pred' q''
        where ((a, _), q') = S.deleteFindMin q
              ans = neighbors a
              (dist', pred', q'') = foldr f (dist, pred, q') ans
              f (an, dn) (d', p', queue) = (d'', p'', queue')
                  where dista = case M.lookup a dist of
                            Nothing -> error "distance to a doesn't exist"
                            Just da -> da
                        maybeDistan = M.lookup an dist
                        alt = dista + dn
                        shouldUpdate = case maybeDistan of
                            Nothing -> True
                            Just dan -> alt < dan
                        (d'', p'', queue') | shouldUpdate = (M.insert an alt d', M.insert an a p', S.insert (an, alt) queue)
                                           | otherwise = (d', p', queue)

amphipodMove :: Area -> Area -> (Area -> [(Area, Int)]) -> Maybe Int
amphipodMove init fin ns = M.lookup fin dist
    where (dist, _) = dijkstras ns dist pred q
              where dist = M.singleton init 0
                    pred = M.empty
                    q = S.singleton (init, 0)

solve' :: Area -> Area -> Int
solve' a final = case amphipodMove a final neighbors of
    Nothing -> error "couldn't reach the final state"
    Just d -> d
