module TwentyTwentyOne.TwentyThirdDecemberP2 where

import Data.Bifunctor (second)
import Data.List (dropWhileEnd, transpose)
import Data.Map (Map)
import qualified Data.Map as M (delete, difference, elems, empty, filterWithKey, findMin, fromList, fromListWith, insert, insertWith, lookup, size, toList, (!))
import Data.Maybe (maybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S (difference, empty, foldl, fromList, insert, intersection, map, member, null, singleton, toList, union, unions)
import qualified Data.Text as T (pack, splitOn, unpack)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V (findIndices, fromList, head, last, null, reverse, slice, tail, toList)
import Debug.Trace

data Anphipod = Amber | Bronze | Copper | Desert deriving (Eq, Ord)

data Room = A Space Space Space Space | B Space Space Space Space | C Space Space Space Space | D Space Space Space Space deriving (Eq, Ord)

data Space = Empty | Occupied Anphipod | RoomEntry Room deriving (Eq, Ord)

type Hallway = Vector Space

energy :: Anphipod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

-- Moves ------------------------------------------------------

-- TODO:
-- DONE create a file (or do it at the end of this) that has all the steps of the optimal test case solution
-- parse it to get all the valid Hallways
-- Implement a single entry and exit strategy
-- hardcode a test that: chains those steps, at each step search for the expected hallway, select it, run the next step. till the end
-- once the test works, extract the algorithm for the real input
-- Check the right answer on AoC

twentyThirdDecemberSolution2 :: Int
twentyThirdDecemberSolution2 = undefined -- solution inputHallway

test = undefined -- solution testHallway

-- Utilities -----------------------------------
getRoom h i = unsafeGetRoom $ h ! i

getRoomA :: Hallway -> Room
getRoomA h = getRoom h roomAIndex

roomAIndex = 2

getRoomB :: Hallway -> Room
getRoomB h = getRoom h roomBIndex

roomBIndex = 4

getRoomC :: Hallway -> Room
getRoomC h = getRoom h roomCIndex

roomCIndex = 6

getRoomD :: Hallway -> Room
getRoomD h = getRoom h roomDIndex

roomDIndex = 8

roomIndices = [roomAIndex, roomBIndex, roomCIndex, roomDIndex]

extractAnphipodFromRoom :: Room -> Maybe (Anphipod, Room, Int)
extractAnphipodFromRoom (A (Occupied a) b c d) = Just (a, A Empty b c d, 1)
extractAnphipodFromRoom (A Empty (Occupied a) c d) = Just (a, A Empty Empty c d, 2)
extractAnphipodFromRoom (A Empty Empty (Occupied a) d) = Just (a, A Empty Empty Empty d, 3)
extractAnphipodFromRoom (A Empty Empty Empty (Occupied a)) = Just (a, A Empty Empty Empty Empty, 4)
extractAnphipodFromRoom A {} = Nothing
extractAnphipodFromRoom (B (Occupied a) b c d) = Just (a, B Empty b c d, 1)
extractAnphipodFromRoom (B Empty (Occupied a) c d) = Just (a, B Empty Empty c d, 2)
extractAnphipodFromRoom (B Empty Empty (Occupied a) d) = Just (a, B Empty Empty Empty d, 3)
extractAnphipodFromRoom (B Empty Empty Empty (Occupied a)) = Just (a, B Empty Empty Empty Empty, 4)
extractAnphipodFromRoom B {} = Nothing
extractAnphipodFromRoom (C (Occupied a) b c d) = Just (a, C Empty b c d, 1)
extractAnphipodFromRoom (C Empty (Occupied a) c d) = Just (a, C Empty Empty c d, 2)
extractAnphipodFromRoom (C Empty Empty (Occupied a) d) = Just (a, C Empty Empty Empty d, 3)
extractAnphipodFromRoom (C Empty Empty Empty (Occupied a)) = Just (a, C Empty Empty Empty Empty, 4)
extractAnphipodFromRoom C {} = Nothing
extractAnphipodFromRoom (D (Occupied a) b c d) = Just (a, D Empty b c d, 1)
extractAnphipodFromRoom (D Empty (Occupied a) c d) = Just (a, D Empty Empty c d, 2)
extractAnphipodFromRoom (D Empty Empty (Occupied a) d) = Just (a, D Empty Empty Empty d, 3)
extractAnphipodFromRoom (D Empty Empty Empty (Occupied a)) = Just (a, D Empty Empty Empty Empty, 4)
extractAnphipodFromRoom D {} = Nothing

insertAnphipodInRoom :: Anphipod -> Room -> Maybe (Room, Int)
insertAnphipodInRoom a (A Empty Empty Empty Empty) = Just (A Empty Empty Empty (Occupied a), 4)
insertAnphipodInRoom a (A Empty Empty Empty d) = Just (A Empty Empty (Occupied a) d, 3)
insertAnphipodInRoom a (A Empty Empty c d) = Just (A Empty (Occupied a) c d, 2)
insertAnphipodInRoom a (A Empty b c d) = Just (A (Occupied a) b c d, 1)
insertAnphipodInRoom a A {} = Nothing
insertAnphipodInRoom a (B Empty Empty Empty Empty) = Just (B Empty Empty Empty (Occupied a), 4)
insertAnphipodInRoom a (B Empty Empty Empty d) = Just (B Empty Empty (Occupied a) d, 3)
insertAnphipodInRoom a (B Empty Empty c d) = Just (B Empty (Occupied a) c d, 2)
insertAnphipodInRoom a (B Empty b c d) = Just (B (Occupied a) b c d, 1)
insertAnphipodInRoom a B {} = Nothing
insertAnphipodInRoom a (C Empty Empty Empty Empty) = Just (C Empty Empty Empty (Occupied a), 4)
insertAnphipodInRoom a (C Empty Empty Empty d) = Just (C Empty Empty (Occupied a) d, 3)
insertAnphipodInRoom a (C Empty Empty c d) = Just (C Empty (Occupied a) c d, 2)
insertAnphipodInRoom a (C Empty b c d) = Just (C (Occupied a) b c d, 1)
insertAnphipodInRoom a C {} = Nothing
insertAnphipodInRoom a (D Empty Empty Empty Empty) = Just (D Empty Empty Empty (Occupied a), 4)
insertAnphipodInRoom a (D Empty Empty Empty d) = Just (D Empty Empty (Occupied a) d, 3)
insertAnphipodInRoom a (D Empty Empty c d) = Just (D Empty (Occupied a) c d, 2)
insertAnphipodInRoom a (D Empty b c d) = Just (D (Occupied a) b c d, 1)
insertAnphipodInRoom a D {} = Nothing

hallwayTakePath :: Hallway -> Int -> Int -> Vector Space
hallwayTakePath h start end = if start > end then V.reverse path else path
  where
    path = V.slice (min start end) (abs (end - start) + 1) h

hallwayValidatePath :: (Vector Space -> Bool) -> Hallway -> Int -> Int -> Bool
hallwayValidatePath validationF h start end = validationF $ hallwayTakePath h start end

hallwayValidatePathExitRoom room = hallwayValidatePath (pathValidatioConditionExitRoom room)

hallwayValidatePathEnterRoom anphi room = hallwayValidatePath (pathValidatioConditionEnterRoom anphi room)

pathValidatioConditionExitRoom :: Room -> Vector Space -> Bool
pathValidatioConditionExitRoom room path =
  isEmpty (V.last path)
    && isRoom (V.head path)
    && room' == room
    && not (isRoomDone room')
    && not (isGoodRoom room')
    && all (\s -> isEmpty s || isRoom s) path
  where
    room' = unsafeGetRoom (V.head path)

pathValidatioConditionEnterRoom :: Anphipod -> Room -> Vector Space -> Bool
pathValidatioConditionEnterRoom anphipod room path =
  isOccupied (V.head path)
    && unsafeGetOccupied (V.head path) == anphipod
    && isRoom (V.last path)
    && unsafeGetRoom (V.last path) == room
    && anphipodOwnRoom anphipod room
    && hasRoomSpace room
    && all (\s -> isEmpty s || isRoom s) (V.tail path)

unsafeGetRoom :: Space -> Room
unsafeGetRoom (RoomEntry r) = r

unsafeGetOccupied :: Space -> Anphipod
unsafeGetOccupied (Occupied a) = a

emptyIndices :: Hallway -> Vector Int
emptyIndices = V.findIndices isEmpty

occupiedIndices :: Hallway -> Vector Int
occupiedIndices = V.findIndices isOccupied

isEmpty :: Space -> Bool
isEmpty Empty = True
isEmpty _ = False

isOccupied :: Space -> Bool
isOccupied (Occupied _) = True
isOccupied _ = False

isRoom :: Space -> Bool
isRoom (RoomEntry _) = True
isRoom _ = False

isGoodRoom :: Room -> Bool
isGoodRoom (A Empty (Occupied Amber) (Occupied Amber) (Occupied Amber)) = True
isGoodRoom (A Empty Empty (Occupied Amber) (Occupied Amber)) = True
isGoodRoom (A Empty Empty Empty (Occupied Amber)) = True
isGoodRoom (B Empty (Occupied Bronze) (Occupied Bronze) (Occupied Bronze)) = True
isGoodRoom (B Empty Empty (Occupied Bronze) (Occupied Bronze)) = True
isGoodRoom (B Empty Empty Empty (Occupied Bronze)) = True
isGoodRoom (C Empty (Occupied Copper) (Occupied Copper) (Occupied Copper)) = True
isGoodRoom (C Empty Empty (Occupied Copper) (Occupied Copper)) = True
isGoodRoom (C Empty Empty Empty (Occupied Copper)) = True
isGoodRoom (D Empty (Occupied Desert) (Occupied Desert) (Occupied Desert)) = True
isGoodRoom (D Empty Empty (Occupied Desert) (Occupied Desert)) = True
isGoodRoom (D Empty Empty Empty (Occupied Desert)) = True
isGoodRoom _ = False

anphipodOwnRoom :: Anphipod -> Room -> Bool
anphipodOwnRoom Amber A {} = True
anphipodOwnRoom Bronze B {} = True
anphipodOwnRoom Copper C {} = True
anphipodOwnRoom Desert D {} = True
anphipodOwnRoom _ _ = False

hasRoomSpace :: Room -> Bool
hasRoomSpace (A Empty _ _ _) = True
hasRoomSpace (B Empty _ _ _) = True
hasRoomSpace (C Empty _ _ _) = True
hasRoomSpace (D Empty _ _ _) = True
hasRoomSpace _ = False

isRoomDone :: Room -> Bool
isRoomDone (A (Occupied Amber) (Occupied Amber) (Occupied Amber) (Occupied Amber)) = True
isRoomDone (B (Occupied Bronze) (Occupied Bronze) (Occupied Bronze) (Occupied Bronze)) = True
isRoomDone (C (Occupied Copper) (Occupied Copper) (Occupied Copper) (Occupied Copper)) = True
isRoomDone (D (Occupied Desert) (Occupied Desert) (Occupied Desert) (Occupied Desert)) = True
isRoomDone _ = False

allRoomsDone :: Hallway -> Bool
allRoomsDone h = isRoomDone (getRoomA h) && isRoomDone (getRoomB h) && isRoomDone (getRoomC h) && isRoomDone (getRoomD h)

input :: IO String
input = readFile "input/2021/23December.txt"

parseInput :: String -> Hallway
parseInput = (\l -> (//) ((V.fromList . fmap (const Empty)) (head l)) ((zip roomIndices . parseRooms) (tail l))) . filter (not . null) . fmap removeSpacesAndHash . lines
  where
    removeSpacesAndHash [] = []
    removeSpacesAndHash (x : xs) = if x == '#' || x == ' ' then removeSpacesAndHash xs else x : removeSpacesAndHash xs
    parseRooms =
      fmap (\(room, as) -> if length as == 2 then RoomEntry (setRoom room (as !! 0) (as !! 1)) else RoomEntry (setRoom' room (as !! 0) (as !! 1) (as !! 2) (as !! 3)))
        . zip [A Empty (Occupied Desert) (Occupied Desert) Empty, B Empty (Occupied Copper) (Occupied Bronze) Empty, C Empty (Occupied Bronze) (Occupied Amber) Empty, D Empty (Occupied Amber) (Occupied Copper) Empty]
        . fmap (fmap (\x -> read [x] :: Anphipod))
        . transpose

setRoom :: Room -> Anphipod -> Anphipod -> Room
setRoom (A _ b c _) a d = A (Occupied a) b c (Occupied d)
setRoom (B _ b c _) a d = B (Occupied a) b c (Occupied d)
setRoom (C _ b c _) a d = C (Occupied a) b c (Occupied d)
setRoom (D _ b c _) a d = D (Occupied a) b c (Occupied d)

setRoom' :: Room -> Anphipod -> Anphipod -> Anphipod -> Anphipod -> Room
setRoom' (A _ _ _ _) a b c d = A (Occupied a) (Occupied b) (Occupied c) (Occupied d)
setRoom' (B _ _ _ _) a b c d = B (Occupied a) (Occupied b) (Occupied c) (Occupied d)
setRoom' (C _ _ _ _) a b c d = C (Occupied a) (Occupied b) (Occupied c) (Occupied d)
setRoom' (D _ _ _ _) a b c d = D (Occupied a) (Occupied b) (Occupied c) (Occupied d)

inputTest :: String
inputTest =
  "#############\n\
  \#...........#\n\
  \###B#C#B#D###\n\
  \  #A#D#C#A#\n\
  \  #########"

--inputTest' :: IO [Hallway]
inputTest' = (fmap (parseInput . T.unpack) . T.splitOn (T.pack "\n\n") . T.pack) <$> readFile "input/2021/21DecemberTest.txt"

instance Show Space where
  show Empty = "."
  show (Occupied a) = "<" ++ show a ++ ">"
  show (RoomEntry r) = "[" ++ show r ++ "]"

instance Show Anphipod where
  show Amber = "A"
  show Bronze = "B"
  show Copper = "C"
  show Desert = "D"

instance Show Room where
  show (A s s' s'' s''') = "A>" ++ show s ++ "," ++ show s' ++ "," ++ show s'' ++ "," ++ show s'''
  show (B s s' s'' s''') = "B>" ++ show s ++ "," ++ show s' ++ "," ++ show s'' ++ "," ++ show s'''
  show (C s s' s'' s''') = "C>" ++ show s ++ "," ++ show s' ++ "," ++ show s'' ++ "," ++ show s'''
  show (D s s' s'' s''') = "D>" ++ show s ++ "," ++ show s' ++ "," ++ show s'' ++ "," ++ show s'''

instance Read Anphipod where
  readsPrec _ = readsAnphipod

readsAnphipod :: ReadS Anphipod
readsAnphipod ('A' : xs) = [(Amber, xs)]
readsAnphipod ('B' : xs) = [(Bronze, xs)]
readsAnphipod ('C' : xs) = [(Copper, xs)]
readsAnphipod ('D' : xs) = [(Desert, xs)]
readsAnphipod _ = []
