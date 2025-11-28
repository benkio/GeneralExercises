module TwentyTwentyOne.December23P1 where

import Data.Bifunctor (second)
import Data.Map (Map)
import qualified Data.Map as M (elems, empty, filterWithKey, insertWith, lookup, size)
import Data.Maybe (maybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S (empty, foldl, fromList, insert, map, null, singleton, toList, union, unions)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V (findIndices, fromList, head, last, null, reverse, slice, tail, toList)
import Debug.Trace

data Anphipod = Amber | Bronze | Copper | Desert deriving (Eq, Ord)

data Room = A Space Space | B Space Space | C Space Space | D Space Space deriving (Eq, Ord)

data Space = Empty | Occupied Anphipod | RoomEntry Room deriving (Eq, Ord)

type Hallway = Vector Space

energy :: Anphipod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

-- Moves ------------------------------------------------------

exitRoom :: Int -> Hallway -> Set (Hallway, Int)
exitRoom roomIndex h = S.fromList $ do
    (anphi, room, outOfRoomSteps) <- (maybeToList . extractAnphipodFromRoom . getRoom h) roomIndex
    destinationIndex <- [end | end <- (V.toList . emptyIndices) h, hallwayValidatePathExitRoom h roomIndex end]
    let h' = h // [(roomIndex, RoomEntry room), (destinationIndex, Occupied anphi)]
        energySpent = energy anphi * (outOfRoomSteps + abs (destinationIndex - roomIndex))
    return (h', energySpent)

exitRoomA :: Hallway -> Set (Hallway, Int)
exitRoomA = exitRoom roomAIndex

exitRoomB :: Hallway -> Set (Hallway, Int)
exitRoomB = exitRoom roomBIndex

exitRoomC :: Hallway -> Set (Hallway, Int)
exitRoomC = exitRoom roomCIndex

exitRoomD :: Hallway -> Set (Hallway, Int)
exitRoomD = exitRoom roomDIndex

exitRoomMoves :: Hallway -> Set (Hallway, Int)
exitRoomMoves h = foldl (\acc f -> acc `S.union` f h) S.empty [exitRoomA, exitRoomB, exitRoomC, exitRoomD]

enterRoomMoves :: Hallway -> Set (Hallway, Int)
enterRoomMoves h = S.fromList $ do
    occupiedHallwaySpot <- V.toList $ occupiedIndices h
    let anphipod = unsafeGetOccupied (h ! occupiedHallwaySpot)
    destinationIndex <- [end | end <- roomIndices, hallwayValidatePathEnterRoom anphipod (unsafeGetRoom (h ! end)) h occupiedHallwaySpot end]
    (room', intoRoomSteps) <- maybeToList $ insertAnphipodInRoom anphipod (unsafeGetRoom (h ! destinationIndex))
    let h' = h // [(occupiedHallwaySpot, Empty), (destinationIndex, RoomEntry room')]
        energySpent = energy anphipod * (intoRoomSteps + abs (destinationIndex - occupiedHallwaySpot))
    return (h', energySpent)

removeUpdateVisitedStates :: Map Hallway Int -> Set (Hallway, Int) -> (Map Hallway Int, Set (Hallway, Int))
removeUpdateVisitedStates cache =
    S.foldl
        ( \(m, acc) (h, e) ->
            if maybe False (<= e) (M.lookup h m)
                then (m, acc)
                else (M.insertWith min h e m, S.insert (h, e) acc)
        )
        (cache, S.empty)

allPossibleMovesStep :: Map Hallway Int -> Set (Hallway, Int) -> (Map Hallway Int, Set (Hallway, Int))
allPossibleMovesStep cache states =
    -- traceShow (length states, M.size cache) $
    removeUpdateVisitedStates cache nextMoves
  where
    nextMoves = (S.unions . S.map (\(h, e) -> S.map (second (+ e)) (enterRoomMoves h `S.union` exitRoomMoves h))) states

allPossibleMoves :: Hallway -> (Map Hallway Int, Set (Hallway, Int))
allPossibleMoves h = until (\(_, states) -> S.null states) (uncurry allPossibleMovesStep) (M.empty, S.singleton (h, 0))

solution :: Hallway -> Int
solution = maximum . M.elems . M.filterWithKey (\h _ -> allRoomsDone h) . fst . allPossibleMoves

input :: IO String
input = readFile "input/2021/23December.txt"

twentyThirdDecemberSolution1 :: Int
twentyThirdDecemberSolution1 = solution inputHallway

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
extractAnphipodFromRoom (A (Occupied a) down) = Just (a, A Empty down, 1)
extractAnphipodFromRoom (A Empty (Occupied b)) = Just (b, A Empty Empty, 2)
extractAnphipodFromRoom (A _ _) = Nothing
extractAnphipodFromRoom (B (Occupied a) down) = Just (a, B Empty down, 1)
extractAnphipodFromRoom (B Empty (Occupied b)) = Just (b, B Empty Empty, 2)
extractAnphipodFromRoom (B _ _) = Nothing
extractAnphipodFromRoom (C (Occupied a) down) = Just (a, C Empty down, 1)
extractAnphipodFromRoom (C Empty (Occupied b)) = Just (b, C Empty Empty, 2)
extractAnphipodFromRoom (C _ _) = Nothing
extractAnphipodFromRoom (D (Occupied a) down) = Just (a, D Empty down, 1)
extractAnphipodFromRoom (D Empty (Occupied b)) = Just (b, D Empty Empty, 2)
extractAnphipodFromRoom (D _ _) = Nothing

insertAnphipodInRoom :: Anphipod -> Room -> Maybe (Room, Int)
insertAnphipodInRoom a (A Empty Empty) = Just (A Empty (Occupied a), 2)
insertAnphipodInRoom a (A Empty down) = Just (A (Occupied a) down, 1)
insertAnphipodInRoom a (A _ _) = Nothing
insertAnphipodInRoom a (B Empty Empty) = Just (B Empty (Occupied a), 2)
insertAnphipodInRoom a (B Empty down) = Just (B (Occupied a) down, 1)
insertAnphipodInRoom a (B _ _) = Nothing
insertAnphipodInRoom a (C Empty Empty) = Just (C Empty (Occupied a), 2)
insertAnphipodInRoom a (C Empty down) = Just (C (Occupied a) down, 1)
insertAnphipodInRoom a (C _ _) = Nothing
insertAnphipodInRoom a (D Empty Empty) = Just (D Empty (Occupied a), 2)
insertAnphipodInRoom a (D Empty down) = Just (D (Occupied a) down, 1)
insertAnphipodInRoom a (D _ _) = Nothing

hallwayTakePath :: Hallway -> Int -> Int -> Vector Space
hallwayTakePath h start end = if start > end then V.reverse path else path
  where
    path = V.slice (min start end) (abs (end - start) + 1) h

hallwayValidatePath :: (Vector Space -> Bool) -> Hallway -> Int -> Int -> Bool
hallwayValidatePath validationF h start end = validationF $ hallwayTakePath h start end

hallwayValidatePathExitRoom = hallwayValidatePath pathValidatioConditionExitRoom

hallwayValidatePathEnterRoom anphi room = hallwayValidatePath (pathValidatioConditionEnterRoom anphi room)

pathValidatioConditionExitRoom :: Vector Space -> Bool
pathValidatioConditionExitRoom path =
    isEmpty (V.last path)
        && isRoom (V.head path)
        && not (isRoomDone (unsafeGetRoom (V.head path)))
        && all (\s -> isEmpty s || isRoom s) path

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

anphipodOwnRoom :: Anphipod -> Room -> Bool
anphipodOwnRoom Amber (A _ _) = True
anphipodOwnRoom Bronze (B _ _) = True
anphipodOwnRoom Copper (C _ _) = True
anphipodOwnRoom Desert (D _ _) = True
anphipodOwnRoom _ _ = False

hasRoomSpace :: Room -> Bool
hasRoomSpace (A Empty Empty) = True
hasRoomSpace (A Empty (Occupied b)) = True
hasRoomSpace (B Empty Empty) = True
hasRoomSpace (B Empty (Occupied b)) = True
hasRoomSpace (C Empty Empty) = True
hasRoomSpace (C Empty (Occupied b)) = True
hasRoomSpace (D Empty Empty) = True
hasRoomSpace (D Empty (Occupied b)) = True
hasRoomSpace _ = False

isRoomDone :: Room -> Bool
isRoomDone (A (Occupied Amber) (Occupied Amber)) = True
isRoomDone (B (Occupied Bronze) (Occupied Bronze)) = True
isRoomDone (C (Occupied Copper) (Occupied Copper)) = True
isRoomDone (D (Occupied Desert) (Occupied Desert)) = True
isRoomDone _ = False

allRoomsDone :: Hallway -> Bool
allRoomsDone h = isRoomDone (getRoomA h) && isRoomDone (getRoomB h) && isRoomDone (getRoomC h) && isRoomDone (getRoomD h)

inputHallway :: Hallway
inputHallway =
    V.fromList
        [ Empty
        , Empty
        , RoomEntry (A (Occupied Amber) (Occupied Copper))
        , Empty
        , RoomEntry (B (Occupied Desert) (Occupied Desert))
        , Empty
        , RoomEntry (C (Occupied Copper) (Occupied Bronze))
        , Empty
        , RoomEntry (D (Occupied Amber) (Occupied Bronze))
        , Empty
        , Empty
        ]

testHallway :: Hallway
testHallway =
    V.fromList
        [ Empty
        , Empty
        , RoomEntry (A (Occupied Bronze) (Occupied Amber))
        , Empty
        , RoomEntry (B (Occupied Copper) (Occupied Desert))
        , Empty
        , RoomEntry (C (Occupied Bronze) (Occupied Copper))
        , Empty
        , RoomEntry (D (Occupied Desert) (Occupied Amber))
        , Empty
        , Empty
        ]

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
    show (A s s') = show s ++ "," ++ show s'
    show (B s s') = show s ++ "," ++ show s'
    show (C s s') = show s ++ "," ++ show s'
    show (D s s') = show s ++ "," ++ show s'
