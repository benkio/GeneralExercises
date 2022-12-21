module TwentyTwentyTwo.SixteenthDecember where

import Data.Bifunctor (second)
import Data.List (groupBy, maximumBy, minimumBy, partition, sortOn, (\\))
import Data.Map (Map, delete, fromList, (!))
import Debug.Trace

data Valve = Valve
    { name :: String
    , rate :: Int
    , connections :: [String]
    }
    deriving (Show, Eq, Ord)

data State = State
    { activeValves :: [Valve]
    , timeLeft :: Int
    , currentPosition :: Valve
    }
    deriving (Show)

initialState :: Valve -> State
initialState cp = State{activeValves = [], timeLeft = 30, currentPosition = cp}

type ValveMap = Map String Valve

input :: IO ValveMap
input = parseInput <$> readFile "input/2022/16December.txt"

-- valves reacheable from the input valve
-- With time to reach them and activate them
-- and through which next pipe name
reachableValves :: ([String], Int) -> Valve -> ValveMap -> [([String], Int, Valve)]
reachableValves prev v vm = fmap (minimumBy (\(_, c, _) (_, c', _) -> c `compare` c')) . groupBy (\(_, _, v) (_, _, v') -> v == v') . sortOn (\(_, _, v) -> v) $ reachableValves' prev v vm

reachableValves' :: ([String], Int) -> Valve -> ValveMap -> [([String], Int, Valve)]
reachableValves' (prevValves, cost) v@Valve{connections = cv, name = vn} vm =
    (fmap (\v -> (prevValves ++ [vn, name v], cost + 2, v)) nextValvesNonEmpty) ++ keepLooking
  where
    (nextValvesEmpty, nextValvesNonEmpty) = (partition ((== 0) . rate) . fmap (vm !) . (\\ prevValves)) cv
    keepLooking = concatMap (\x -> reachableValves' (prevValves ++ [vn], cost + 1) x vm) (nextValvesEmpty ++ nextValvesNonEmpty)

selectPathToValve :: Int -> [([String], Int, Valve)] -> [String]
selectPathToValve _ [] = []
selectPathToValve timeLeft cs = ((\(p, _, _) -> tail p) . maximumBy (\(_, c, v) (_, c', v') -> candidatePotentialFlow timeLeft c v `compare` candidatePotentialFlow timeLeft c' v')) cs

candidatePotentialFlow :: Int -> Int -> Valve -> Int
candidatePotentialFlow timeLeft stepsToActivateValve (Valve{rate = r}) =
    (timeLeft - stepsToActivateValve) * r

moveToValve :: State -> ValveMap -> State
moveToValve st@State{currentPosition = cp, activeValves = av, timeLeft = tl} vm =
    if cp `notElem` av && rate cp > 0
        then st{activeValves = av ++ [cp], timeLeft = tl - 1}
        else st{currentPosition = nextValve, timeLeft = tl - 1}
  where
    candidates = filter (\(_, _, v) -> v `notElem` av) $ reachableValves ([], 0) cp (delete (name cp) vm)
    path = selectPathToValve tl candidates
    nextValve = if null path then cp else vm ! (head path)

evolve :: ValveMap -> [State]
evolve vm = (takeWhile ((> 0) . timeLeft) . iterate (`moveToValve` vm) . initialState) (vm ! "AA")

releasedPressure :: State -> Int
releasedPressure (State{activeValves = av}) = sum $ fmap rate av

solution1 :: ValveMap -> Int
solution1 =
    -- fmap (\s -> (releasedPressure s, s))
    sum . fmap releasedPressure . evolve

sixteenthDecemberSolution1 :: IO Int
sixteenthDecemberSolution1 = undefined

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
