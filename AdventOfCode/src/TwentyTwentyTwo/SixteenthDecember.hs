module TwentyTwentyTwo.SixteenthDecember where

import Text.Printf (printf)

import Data.Bifunctor (second)
import Data.List (groupBy, maximumBy, minimumBy, partition, sort, sortOn, (\\))
import Data.Map (Map, delete, elems, fromList, keys, size, (!))
import Debug.Trace

data Valve = Valve
    { name :: String
    , rate :: Int
    , connections :: [String]
    }
    deriving (Show, Eq, Ord)

data State = State
    { valves :: [String]
    , timeSpent :: Int
    , total :: Int
    }
    deriving (Show)

initialState :: (String, String) -> Int -> Int -> State
initialState (sv, ev) c t = State{valves = [sv, ev], timeSpent = c, total = t}

addStepState :: ValveMap -> (String, String) -> (Int, Int) -> State -> State
addStepState vm (sv, ev) (c, r) s =
    State
        { valves = [sv] ++ valves s
        , timeSpent = c + timeSpent s
        , total = (((rate . (vm !))) sv * timeSpent s) + r + total s
        }

type ValveMap = Map String Valve
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
    (maximumBy (\s s' -> total s `compare` total s') . fmap (extendTo30Time vm)) $ innerSearch "AA" vm cm ["AA"] (keys cm)

innerSearch :: String -> ValveMap -> ConnectionCostMap -> [String] -> [(String, String)] ->[State]
innerSearch starting vm cm visited ks =
    ( take (size cm * 2)
        . reverse
        . sortOn total
        . concatMap
            ( \(x, y) ->
                let ss = innerSearch y vm cm (visited ++ [x,y]) ks
                 in if null ss
                        then [(uncurry (initialState (x, y)) . (cm !)) (x, y)]
                        else (filter ((<= 30) . timeSpent) . fmap (addStepState vm (x, y) (cm ! (x, y)))) ss
            )
        . filter (\(s, e) -> s == starting && e `notElem` visited)
    )
        ks

releasedPressure :: ValveMap -> [String] -> Int
releasedPressure vm av = sum $ fmap (rate . (vm !)) av

extendTo30Time :: ValveMap -> State -> State
extendTo30Time vm (State{timeSpent = ts, total = t, valves = vs}) =
    State
        { timeSpent = 30
        , valves = vs
        , total = t + ((30 - ts) * releasedPressure vm vs)
        }

solution1 :: ValveMap -> Int
solution1 vm = (total . search vm. connectionMap) vm

sixteenthDecemberSolution1 :: IO Int
sixteenthDecemberSolution1 = solution1 <$> input -- return (solution1 testInput)

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
