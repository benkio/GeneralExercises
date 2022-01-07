module TwentyTwentyOne.TwentyThirdDecemberP2 where

import Data.Bifunctor (first, second)
import Data.List (dropWhileEnd, find, transpose, (\\), minimumBy)
import Data.Map (Map)
import qualified Data.Map as M (delete, deleteMin, difference, elems, empty, filterWithKey, findMin, fromList, fromListWith, insert, insertWith, keys, lookup, member, singleton, size, toList)
import Data.Maybe (fromJust, maybe, maybeToList, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S (difference, empty, foldl, fromList, insert, intersection, map, member, null, singleton, size, toList, union, unions, delete, filter)
import qualified Data.Text as T (pack, splitOn, unpack)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V (findIndices, fromList, head, last, null, reverse, slice, tail, toList)
import Debug.Trace

data Anphipod = Amber | Bronze | Copper | Desert
  deriving
    ( Eq,
      Ord
    )

data Room = A [Space] | B [Space] | C [Space] | D [Space]
  deriving
    ( Eq,
      Ord
    )

data Space = Empty | Occupied Anphipod | RoomEntry Room
  deriving
    ( Eq,
      Ord
    )

data State = State
  { openSet :: Map Int Hallway, -- The int is the Fscore: gScore(n) + h(n)
    cameFrom :: Map Hallway Hallway,
    gScore :: Map Hallway Int
  }
  deriving (Show)

type Hallway = Vector Space

energy :: Anphipod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

initialState :: Hallway -> State
initialState h = State {openSet = M.singleton (distanceFromGoal h) h, cameFrom = M.empty, gScore = M.singleton h 0}

-- TODO:
-- DONE create a file (or do it at the end of this) that has all the steps of the optimal test case solution
-- DONE parse it to get all the valid Hallways
-- DONE Implement this https://en.wikipedia.org/wiki/A*_search_algorithm - the heuristic should be the difference with the full house, counting the type of anphipod
-- DONE hardcode a test that: chains those steps, at each step search for the expected hallway, select it, run the next step. till the end
-- DONE once the test works, extract the algorithm for the real input
-- Debug why the algorithm stops instead of reaching the end
-- Check the right answer on AoC

selectNextMin :: State -> Hallway
selectNextMin State {openSet = op, gScore = gs}
 | S.null op = error "can't find the minimum of an empty set"
 | otherwise = (
     snd
     . minimumBy (\(x, _) (x',_) -> x `compare` x')
     . mapMaybe (\h -> fmap (\x -> (x,h)) (M.lookup h gs))
     . S.toList) op

reconstructPath :: Map Hallway Hallway -> [Hallway] -> [Hallway]
reconstructPath comeFrom ps@(p:_) = foldl (\ps' prev -> reconstructPath comeFrom (prev:ps') ) ps $ M.lookup p comeFrom

aStar :: State -> State
aStar =
  until
    (\s -> let emptyOpenset = (null . openSet) s
               goalReached = (allRoomsDone . snd . M.findMin . openSet) s
           in traceShow ("emptyOpenSet " ++ (show emptyOpenset) ++ " - goalReached " ++ (show goalReached)) (emptyOpenset || goalReached)
    )
    ( \s ->
        -- traceShow (((show . length . openSet) s) ++ " - " ++ (show . null . openSet) s  ++ " - " ++  (show . allRoomsDone . snd . M.findMin . openSet) s)
        (aStarStep s)
    )

aStarStep :: State -> State
aStarStep s@(State {openSet = op}) =
    let nextHallwayNeighboors = nextStates nextHallway
        (op', cf, gs) = computeNeighboors nextHallwayNeighboors nextHallway nextState
     in State {openSet = op', cameFrom = cf, gScore = gs}
  where
    nextHallway = (snd . M.findMin) op
    nextState = s {openSet = M.deleteMin op}

-- Given the set of neighboors and their distance from current, the current hallway and the state, update the state accordingly to A*
computeNeighboors :: Set (Int, Hallway) -> Hallway -> State -> (Map Int Hallway, Map Hallway Hallway, Map Hallway Int)
computeNeighboors neighboors current (State {openSet = op, cameFrom = cf, gScore = gs}) =
  S.foldl
    ( \(op', cf', gs') (dn, n) ->
        let g = fromJust (M.lookup current gs') + dn
            r = (M.insert (g + distanceFromGoal n) n op', M.insert n current cf', M.insert n g gs')
         in maybe r (\g' -> if g < g' then r else (op', cf', gs')) $ M.lookup n gs
    )
    (op, cf, gs)
    neighboors

exitRoom :: Int -> Hallway -> Set (Int, Hallway)
exitRoom roomIndex h = S.fromList $ do
  (anphi, room, outOfRoomSteps) <- (maybeToList . extractAnphipodFromRoom . getRoom h) roomIndex
  destinationIndex <- [end | end <- (V.toList . emptyIndices) h, hallwayValidatePathExitRoom room h roomIndex end]
  let h' = h // [(roomIndex, RoomEntry room), (destinationIndex, Occupied anphi)]
      energySpent = energy anphi * (outOfRoomSteps + abs (destinationIndex - roomIndex))
  return (energySpent, h')

exitRoomA :: Hallway -> Set (Int, Hallway)
exitRoomA = exitRoom roomAIndex

exitRoomB :: Hallway -> Set (Int, Hallway)
exitRoomB = exitRoom roomBIndex

exitRoomC :: Hallway -> Set (Int, Hallway)
exitRoomC = exitRoom roomCIndex

exitRoomD :: Hallway -> Set (Int, Hallway)
exitRoomD = exitRoom roomDIndex

allMoves :: Hallway -> Set (Int, Hallway)
allMoves h = foldl (\acc f -> acc `S.union` f h) S.empty [exitRoomA, exitRoomB, exitRoomC, exitRoomD, enterRoomMoves]

enterRoomMoves :: Hallway -> Set (Int, Hallway)
enterRoomMoves h = S.fromList $ do
  occupiedHallwaySpot <- V.toList $ occupiedIndices h
  let anphipod = unsafeGetOccupied (h ! occupiedHallwaySpot)
  destinationIndex <- [end | end <- roomIndices, hallwayValidatePathEnterRoom anphipod (unsafeGetRoom (h ! end)) h occupiedHallwaySpot end]
  (room', intoRoomSteps) <- maybeToList $ insertAnphipodInRoom anphipod (unsafeGetRoom (h ! destinationIndex))
  let h' = h // [(occupiedHallwaySpot, Empty), (destinationIndex, RoomEntry room')]
      energySpent = energy anphipod * (intoRoomSteps + abs (destinationIndex - occupiedHallwaySpot))
  return (energySpent, h')

twentyThirdDecemberSolution2 :: Int
twentyThirdDecemberSolution2 = undefined -- solution inputHallway

distanceFromGoal :: Hallway -> Int
distanceFromGoal h =
  sum (fmap (distanceFromGoal' . getRoom h) roomIndices)
    + sum (fmap (\i -> distanceFromGoal'' (unsafeGetOccupied (h ! i), i)) (occupiedIndices h))

distanceFromGoal' :: Room -> Int
distanceFromGoal' (A as) =
  foldl
    ( \acc (a, i) ->
        acc + (if unsafeGetOccupied a == Amber then 0 else energy (unsafeGetOccupied a) * (i + distanceFromGoal'' (unsafeGetOccupied a, roomAIndex)))
    )
    0
    (filter (/= Empty) as `zip` [1 ..])
distanceFromGoal' (B as) =
  foldl
    ( \acc (a, i) ->
        acc + (if unsafeGetOccupied a == Bronze then 0 else energy (unsafeGetOccupied a) * (i + distanceFromGoal'' (unsafeGetOccupied a, roomBIndex)))
    )
    0
    (filter (/= Empty) as `zip` [1 ..])
distanceFromGoal' (C as) =
  foldl
    ( \acc (a, i) ->
        acc + (if unsafeGetOccupied a == Copper then 0 else energy (unsafeGetOccupied a) * (i + distanceFromGoal'' (unsafeGetOccupied a, roomCIndex)))
    )
    0
    (filter (/= Empty) as `zip` [1 ..])
distanceFromGoal' (D as) =
  foldl
    ( \acc (a, i) ->
        acc + (if unsafeGetOccupied a == Desert then 0 else energy (unsafeGetOccupied a) * (i + distanceFromGoal'' (unsafeGetOccupied a, roomDIndex)))
    )
    0
    (filter (/= Empty) as `zip` [1 ..])

distanceFromGoal'' :: (Anphipod, Int) -> Int
distanceFromGoal'' (Amber, i) = energy Amber * abs (roomAIndex - i)
distanceFromGoal'' (Bronze, i) = energy Bronze * abs (roomBIndex - i)
distanceFromGoal'' (Copper, i) = energy Copper * abs (roomCIndex - i)
distanceFromGoal'' (Desert, i) = energy Desert * abs (roomDIndex - i)

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
extractAnphipodFromRoom (A as)
  | any (not . isEmpty) as =
    Just ((unsafeGetOccupied . head . dropWhile isEmpty) as, A ((takeWhile isEmpty as) ++ [Empty] ++ ((tail . dropWhile isEmpty) as)), ((+ 1) . length . takeWhile isEmpty) as)
  | otherwise = Nothing
extractAnphipodFromRoom (B as)
  | any (not . isEmpty) as =
    Just ((unsafeGetOccupied . head . dropWhile isEmpty) as, B ((takeWhile isEmpty as) ++ [Empty] ++ ((tail . dropWhile isEmpty) as)), ((+ 1) . length . takeWhile isEmpty) as)
  | otherwise = Nothing
extractAnphipodFromRoom (C as)
  | any (not . isEmpty) as =
    Just ((unsafeGetOccupied . head . dropWhile isEmpty) as, C ((takeWhile isEmpty as) ++ [Empty] ++ ((tail . dropWhile isEmpty) as)), ((+ 1) . length . takeWhile isEmpty) as)
  | otherwise = Nothing
extractAnphipodFromRoom (D as)
  | any (not . isEmpty) as =
    Just ((unsafeGetOccupied . head . dropWhile isEmpty) as, D ((takeWhile isEmpty as) ++ [Empty] ++ ((tail . dropWhile isEmpty) as)), ((+ 1) . length . takeWhile isEmpty) as)
  | otherwise = Nothing

insertAnphipodInRoom :: Anphipod -> Room -> Maybe (Room, Int)
insertAnphipodInRoom a (A as)
  | any (isEmpty) as =
    Just (A ((tail . takeWhile isEmpty) as ++ (Occupied a) : (dropWhile isEmpty as)), (length . takeWhile isEmpty) as)
  | otherwise = Nothing
insertAnphipodInRoom a (B as)
  | any (isEmpty) as =
    Just (B ((tail . takeWhile isEmpty) as ++ (Occupied a) : (dropWhile isEmpty as)), (length . takeWhile isEmpty) as)
  | otherwise = Nothing
insertAnphipodInRoom a (C as)
  | any (isEmpty) as =
    Just (C ((tail . takeWhile isEmpty) as ++ (Occupied a) : (dropWhile isEmpty as)), (length . takeWhile isEmpty) as)
  | otherwise = Nothing
insertAnphipodInRoom a (D as)
  | any (isEmpty) as =
    Just (D ((tail . takeWhile isEmpty) as ++ (Occupied a) : (dropWhile isEmpty as)), (length . takeWhile isEmpty) as)
  | otherwise = Nothing

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
    && not (isRoomDone room')
    && not (noIntruders room')
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
    && noIntruders room
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
anphipodOwnRoom Amber (A _) = True
anphipodOwnRoom Bronze (B _) = True
anphipodOwnRoom Copper (C _) = True
anphipodOwnRoom Desert (D _) = True
anphipodOwnRoom _ _ = False

hasRoomSpace :: Room -> Bool
hasRoomSpace (A as) = any (isEmpty) as
hasRoomSpace (B as) = any (isEmpty) as
hasRoomSpace (C as) = any (isEmpty) as
hasRoomSpace (D as) = any (isEmpty) as

noIntruders :: Room -> Bool
noIntruders (A as) = all (\x -> isEmpty x || x == (Occupied Amber)) as
noIntruders (B as) = all (\x -> isEmpty x || x == (Occupied Bronze)) as
noIntruders (C as) = all (\x -> isEmpty x || x == (Occupied Copper)) as
noIntruders (D as) = all (\x -> isEmpty x || x == (Occupied Desert)) as

isRoomDone :: Room -> Bool
isRoomDone (A as) = length as == 4 && all (== Occupied Amber) as
isRoomDone (B as) = length as == 4 && all (== Occupied Bronze) as
isRoomDone (C as) = length as == 4 && all (== Occupied Copper) as
isRoomDone (D as) = length as == 4 && all (== Occupied Desert) as

allRoomsDone :: Hallway -> Bool
allRoomsDone h = isRoomDone (getRoomA h) && isRoomDone (getRoomB h) && isRoomDone (getRoomC h) && isRoomDone (getRoomD h)

input :: IO String
input = readFile "input/2021/23December.txt"

parseInput :: String -> Hallway
parseInput =
  (\l -> (//) ((V.fromList . fmap (\x -> read [x] :: Space)) (head l)) ((zip roomIndices . parseRooms) (tail l)))
    . filter (not . null)
    . fmap removeSpacesAndHash
    . lines
  where
    removeSpacesAndHash [] = []
    removeSpacesAndHash (x : xs) = if x == '#' || x == ' ' then removeSpacesAndHash xs else x : removeSpacesAndHash xs
    parseRooms =
      fmap
        ( \(room, as) ->
            if length as == 2
              then RoomEntry (setRoom room (head as) (as !! 1))
              else RoomEntry (setRoom' room as)
        )
        . zip
          [ setRoom' (A []) (fmap Occupied [Desert, Desert]),
            setRoom' (B []) (fmap Occupied [Copper, Bronze]),
            setRoom' (C []) (fmap Occupied [Bronze, Amber]),
            setRoom' (D []) (fmap Occupied [Amber, Copper])
          ]
        . fmap (fmap (\x -> read [x] :: Space))
        . transpose

setRoom :: Room -> Space -> Space -> Room
setRoom (A as) a d = A (a : as ++ [d])
setRoom (B as) a d = B (a : as ++ [d])
setRoom (C as) a d = C (a : as ++ [d])
setRoom (D as) a d = D (a : as ++ [d])

setRoom' :: Room -> [Space] -> Room
setRoom' (A _) as = A as
setRoom' (B _) as = B as
setRoom' (C _) as = C as
setRoom' (D _) as = D as

inputTest :: String
inputTest =
  "#############\n\
  \#...........#\n\
  \###B#C#B#D###\n\
  \  #A#D#C#A#\n\
  \  #########"

inputTest' :: IO [Hallway]
inputTest' = fmap (parseInput . T.unpack) . T.splitOn (T.pack "\n\n") . T.pack <$> readFile "input/2021/21DecemberTest.txt"

-- test (h : hs) = let
--   s = dijkstra $ initialState h
--   goal = (snd . M.findMin . openSet) s
--   in reconstructPath (cameFrom s) [goal]

-- test' (h : hs) = let
--   s = until ((== (head hs)) . snd . M.findMin . openSet) (\x ->
--     let x' = dijkstraStep x
--     in traceShow ((((head hs) `elem`) . M.elems . openSet) x) x'
--     ) (initialState h)
--   op = openSet s
--   nextHallway = (snd . M.findMin) op
--   nextState = s {openSet = M.deleteMin op}
--   nextHallwayNeighboors = allMoves nextHallway
--   -- (op', cf, gs) = computeNeighboors nextHallwayNeighboors nextHallway nextState
--   in (find (== (hs !! 1)) . S.toList . S.map snd) nextHallwayNeighboors



-- test'' (h : hs) = (find ((== head hs) . snd . M.findMin . openSet) . iterate dijkstraStep . initialState) h

instance Show Space where
  show Empty = "."
  show (Occupied a) = "<" ++ show a ++ ">"
  show (RoomEntry r) = show r

instance Show Anphipod where
  show Amber = "A"
  show Bronze = "B"
  show Copper = "C"
  show Desert = "D"

instance Show Room where
  show (A as) = "A>" ++ show as
  show (B as) = "B>" ++ show as
  show (C as) = "C>" ++ show as
  show (D as) = "D>" ++ show as

instance Read Anphipod where
  readsPrec _ = readsAnphipod

instance Read Space where
  readsPrec _ = readsSpace

readsSpace :: ReadS Space
readsSpace ('.' : xs) = [(Empty, xs)]
readsSpace xs = first Occupied <$> readsAnphipod xs

readsAnphipod :: ReadS Anphipod
readsAnphipod ('A' : xs) = [(Amber, xs)]
readsAnphipod ('B' : xs) = [(Bronze, xs)]
readsAnphipod ('C' : xs) = [(Copper, xs)]
readsAnphipod ('D' : xs) = [(Desert, xs)]
readsAnphipod _ = []
