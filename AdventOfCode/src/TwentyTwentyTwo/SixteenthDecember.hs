module TwentyTwentyTwo.SixteenthDecember where

import Text.Printf (printf)

import Data.Bifunctor (second)
import Data.List (groupBy, maximumBy, minimumBy, partition, sort, sortOn, (\\))
import Data.Map (Map, delete, elems, empty, fromList, insert, keys, lookup, size, (!))
import Debug.Trace
import Prelude hiding (lookup)

data Valve = Valve
    { name :: String
    , rate :: Int
    , connections :: [String]
    }
    deriving (Show, Eq, Ord)

data State = State
    { currentValve :: String
    , valves :: [String]
    , timeSpent :: Int
    , total :: Int
    }
    deriving (Show)

initialState :: String -> State
initialState sv = State{currentValve = sv, valves = [], timeSpent = 0, total = 0}

addStepState :: ValveMap -> (String, String) -> (Int, Int) -> State -> State
addStepState vm (sv, ev) (c, r) s =
    State
        { currentValve = ev
        , valves = sv : (valves s)
        , timeSpent = c + timeSpent s
        , total = (releasedPressure vm (valves s)) * c + total s + r
        }

type ValveMap = Map String Valve
type BestPaths = Map String State
type ConnectionCostMap = Map (String, String) (Int, Int)

input :: IO ValveMap
input = parseInput <$> readFile "input/2022/16December.txt"

connectionMap :: ValveMap -> ConnectionCostMap
connectionMap vm = (fromList . innerSearch . filter (\v -> ((> 0) . rate) v || name v == "AA") . elems) vm
  where
    innerSearch :: [Valve] -> [((String, String), (Int, Int))]
    innerSearch [] = []
    innerSearch (v : vs) =
        let vrs = (\(v', c) -> ((name v, name v'), (c, c * rate v))) <$> reachableValves ([], 0) v vm
         in vrs ++ innerSearch vs

reachableValves :: ([String], Int) -> Valve -> ValveMap -> [(Valve, Int)]
reachableValves prev v vm = (fmap ((\(_, c, v) -> (v, c)) . minimumBy (\(_, c, _) (_, c', _) -> c `compare` c')) . groupBy (\(_, _, v) (_, _, v') -> v == v') . sortOn (\(_, _, v) -> v)) $ reachableValves' prev v vm

reachableValves' :: ([String], Int) -> Valve -> ValveMap -> [([String], Int, Valve)]
reachableValves' (prevValves, cost) v@Valve{connections = cv, name = vn} vm =
    (fmap (\v -> (prevValves ++ [vn, name v], cost + 2, v)) nextValvesNonEmpty) ++ keepLooking
  where
    (nextValvesEmpty, nextValvesNonEmpty) = (partition (\v -> (rate v == 0 || name v == "AA")) . fmap (vm !) . (\\ prevValves)) cv
    keepLooking = concatMap (\x -> reachableValves' (prevValves ++ [vn], cost + 1) x vm) (nextValvesEmpty ++ nextValvesNonEmpty)

log' f x = trace (f x) x

search :: ValveMap -> ConnectionCostMap -> State
search vm cm =
    let startingState = initialState "AA"
     in ( maximumBy (\s s' -> total s `compare` total s')
            . fmap (extendToTime vm 30)
            . traceShowId
            . elems
        )
            $ searchR [startingState] empty cm vm

searchR :: [State] -> BestPaths -> ConnectionCostMap -> ValveMap -> BestPaths
searchR sts bsp cm vm =
    let next = sts >>= \s -> nextStep s vm cm bsp
        nextBest = (fmap (bestState vm) . groupBy (\s s' -> currentValve s == currentValve s') . sortOn currentValve) next
        bsb' = foldl (\bs s -> insert (currentValve s) s bs) bsp nextBest
     in if null (log' (show . length) next)
            then bsp
            else searchR next bsb' cm vm

nextStep :: State -> ValveMap -> ConnectionCostMap -> BestPaths -> [State]
nextStep st vm cm bsp =
    ( filter (\s -> (timeSpent s <= 30))
        . fmap (\(x, y) -> addStepState vm (x, y) (cm ! (x, y)) st)
        . filter (\(s, e) -> s == (currentValve st) && e `notElem` (valves st))
        . keys
    )
        cm

compareState :: ValveMap -> State -> State -> Ordering
compareState vm s s' =
    let maxTime = max (timeSpent s) (timeSpent s')
     in total (extendToTime vm maxTime s) `compare` total (extendToTime vm maxTime s')

bestState :: ValveMap -> [State] -> State
bestState _ (s : []) = s
bestState vm (x : y : xs) =
    let s = if compareState vm x y == GT then x else y
     in bestState vm (s : xs)

releasedPressure :: ValveMap -> [String] -> Int
releasedPressure vm av = sum $ fmap (rate . (vm !)) av

extendToTime :: ValveMap -> Int -> State -> State
extendToTime vm time (State{timeSpent = ts, total = t, valves = vs, currentValve = cv}) =
    State
        { currentValve = cv
        , timeSpent = time
        , valves = vs
        , total = t + ((time - ts) * releasedPressure vm (cv : vs))
        }

solution1 :: ValveMap -> Int
solution1 vm = (total . traceShowId . search vm . connectionMap) vm

-- 1909 too low
sixteenthDecemberSolution1 :: IO Int
sixteenthDecemberSolution1 = solution1 <$> input

sixteenthDecemberSolution2 :: IO Int
sixteenthDecemberSolution2 = undefined

parseInput :: String -> ValveMap
parseInput = fromList . fmap parseValve . lines

parseValve :: String -> (String, Valve)
parseValve s = ((take 2 . drop 6) s, Valve{name = parseName, rate = parseRate, connections = parseConnection})
  where
    parseName = (take 2 . drop 6) s
    parseRate = ((\x -> read x :: Int) . takeWhile (/= ';') . drop 23) s
    parseConnection = (parseConnections . drop 5 . words . snd . break (== ';')) s
    parseConnections [] = []
    parseConnections (s : ss) = if last s == ',' then init s : parseConnections ss else s : parseConnections ss

testInput' :: ValveMap
testInput' =
    parseInput
        "Valve AA has flow rate=0; tunnels lead to valves DD, BB\n\
        \Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
        \Valve CC has flow rate=0; tunnels lead to valves DD, BB\n\
        \Valve DD has flow rate=20; tunnels lead to valves CC, AA"

testInput :: ValveMap
testInput =
    parseInput
        "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n\
        \Valve BB has flow rate=13; tunnels lead to valves CC, AA\n\
        \Valve CC has flow rate=2; tunnels lead to valves DD, BB\n\
        \Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n\
        \Valve EE has flow rate=3; tunnels lead to valves FF, DD\n\
        \Valve FF has flow rate=0; tunnels lead to valves EE, GG\n\
        \Valve GG has flow rate=0; tunnels lead to valves FF, HH\n\
        \Valve HH has flow rate=22; tunnel leads to valve GG\n\
        \Valve II has flow rate=0; tunnels lead to valves AA, JJ\n\
        \Valve JJ has flow rate=21; tunnel leads to valve II"
