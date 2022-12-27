module TwentyTwentyTwo.SixteenthDecember where

import Data.Bifunctor (bimap)
import Data.List (groupBy, minimumBy, partition, sortOn, (\\))
import Data.Map (Map, elems, empty, filterWithKey, fromList, toList, unionWith, (!))
import Data.Tree (Tree, foldTree, unfoldTree)
import Debug.Trace
import Text.Printf (printf)

data Valve = Valve
    { name :: String
    , rate :: Int
    , connections :: [String]
    }
    deriving (Eq, Ord)

data State = State
    { currentValve :: Valve
    , openValves :: [Valve]
    , total :: Int
    , cost :: Int
    }

instance Show Valve where
    show v = printf "V: %s %d %s" (name v) (rate v) ((show . connections) v)
instance Show State where
    show s = printf "S: %d %d %s %s" (total s) (cost s) ((name . currentValve) s) ((show . fmap name . openValves) s)

type ValveMap = Map String Valve
type ConnectionCostMap = Map (String, String) (Int, Int)

input :: IO ValveMap
input = parseInput <$> readFile "input/2022/16December.txt"

flowRatesValves :: ValveMap -> [Valve]
flowRatesValves = filter ((> 0) . rate) . elems

startingState :: ValveMap -> State
startingState = (\v -> State{currentValve = v, total = 0, cost = 0, openValves = []}) . (! "AA")

move :: ValveMap -> ConnectionCostMap -> State -> [State]
move vm ccn (State{currentValve = cv, openValves = ovs, total = t, cost = c})
    | length ovs == length (flowRatesValves vm) || c >= 30 = []
    | otherwise = connectionStates
  where
    newTotal c = t + ((sum . fmap rate) ovs) * c
    connectionStates =
        ( fmap (\((_, e), (c, _)) -> buildState e c)
            . toList
            . (\n -> filterWithKey (\(s, e) _ -> s == n && e `notElem` (fmap name ovs)) ccn)
            . name
        )
            cv
    buildState end cost =
        State
            { currentValve = vm ! end
            , cost = c + cost
            , total = newTotal cost
            , openValves = (vm ! end) : ovs
            }

pathTree :: ValveMap -> ConnectionCostMap -> State -> Tree State
pathTree vm ccm s = unfoldTree (\x -> (x, move vm ccm x)) s

extendToTime :: Int -> State -> State
extendToTime time (State{cost = ts, total = t, openValves = vs, currentValve = cv}) =
    State
        { currentValve = cv
        , cost = time
        , openValves = vs
        , total = t + ((time - ts) * ((sum . fmap rate) vs))
        }

solution1 :: ValveMap -> Int
solution1 vm = foldTree comparePaths tree
  where
    tree = pathTree vm (connectionMap vm) (startingState vm)

comparePaths :: State -> [Int] -> Int
comparePaths s [] = (total . extendToTime 30) s
comparePaths _ childs = maximum childs

-- 1947
sixteenthDecemberSolution1 :: IO Int
sixteenthDecemberSolution1 = solution1 <$> input

sixteenthDecemberSolution2 :: IO Int
sixteenthDecemberSolution2 = undefined

-- connectionMap ---------------------------------

connectionMap :: ValveMap -> ConnectionCostMap
connectionMap vm = (innerSearch . filter (\v -> ((> 0) . rate) v || name v == "AA") . elems) vm
  where
    innerSearch :: [Valve] -> ConnectionCostMap
    innerSearch [] = empty
    innerSearch (v : vs) =
        let vrs = fromList $ (\(v', c) -> ((name v, name v'), (c, c * rate v))) <$> reachableValves ([], 0) v vm
         in unionWith
                ( \(c, v) (c', v') -> case c `compare` c' of
                    GT -> (c, v)
                    EQ -> (c, v)
                    LT -> (c', v')
                )
                vrs
                (innerSearch vs)

reachableValves :: ([String], Int) -> Valve -> ValveMap -> [(Valve, Int)]
reachableValves prev v vm = (fmap ((\(_, c, v) -> (v, c)) . minimumBy (\(_, c, _) (_, c', _) -> c `compare` c')) . groupBy (\(_, _, v) (_, _, v') -> v == v') . sortOn (\(_, _, v) -> v)) $ reachableValves' prev v vm

reachableValves' :: ([String], Int) -> Valve -> ValveMap -> [([String], Int, Valve)]
reachableValves' (prevValves, cost) v@Valve{connections = cv, name = vn} vm =
    (fmap (\v -> (prevValves ++ [vn, name v], cost + 2, v)) nextValvesNonEmpty) ++ keepLooking
  where
    (nextValvesEmpty, nextValvesNonEmpty) = (partition (\v -> (rate v == 0 || name v == "AA")) . fmap (vm !) . (\\ prevValves)) cv
    keepLooking = concatMap (\x -> reachableValves' (prevValves ++ [vn], cost + 1) x vm) (nextValvesEmpty ++ nextValvesNonEmpty)

-- Parse -----------------------------------------

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
