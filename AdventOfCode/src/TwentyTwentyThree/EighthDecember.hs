module TwentyTwentyThree.EighthDecember where

import Data.Bifunctor (bimap, first, second)
import Data.Map (Map, elems, fromList)
import qualified Data.Map as M (filter, lookup)
import Data.Maybe (fromJust, maybe)
import Debug.Trace

data Move = L | R deriving (Read, Show, Eq)

data Node = N {nId :: String, nl :: String, nr :: String} deriving (Show)

data DesertMap = DM {moves :: [Move], nodes :: Map String Node} deriving (Show)

input :: IO DesertMap
input = parseInput <$> readFile "input/2023/8December.txt"

parseInput :: String -> DesertMap
parseInput = (\(ms, ns) -> DM{moves = cycle ms, nodes = fromList ns}) . bimap (fmap (\x -> read [x] :: Move) . head) (fmap parseNode . tail) . break (== "") . lines
  where
    parseNode :: String -> (String, Node)
    parseNode = (\[nid, _, nl, nr] -> (nid, N{nId = nid, nl = (init . tail) nl, nr = init nr})) . words

unsafeSearchNode :: String -> Map String Node -> Node
unsafeSearchNode n = fromJust . M.lookup n

mapStep :: (String -> Bool) -> Move -> Node -> Maybe String
mapStep terminalF m n
    | isDestination n = Nothing
    | otherwise = Just $ if m == L then nl n else nr n
  where
    isDestination n = terminalF $ nId n

traverseDesertMap :: (String -> Bool) -> String -> DesertMap -> [Node]
traverseDesertMap terminalF start (DM{moves = ms, nodes = ns}) = go startingPoint ms
  where
    startingPoint = unsafeSearchNode start ns
    go :: Node -> [Move] -> [Node]
    go n (x : xs) = n : maybe [] (\n' -> go (unsafeSearchNode n' ns) xs) (mapStep terminalF x n)

solution1 :: (String -> Bool) -> String -> DesertMap -> (Int, Node)
solution1 terminalF start dm = (((\x -> x - 1) . length) ns, last ns)
  where
    ns = traverseDesertMap terminalF start dm

testInput :: DesertMap
testInput =
    parseInput
        "RL\n\
        \\n\
        \AAA = (BBB, CCC)\n\
        \BBB = (DDD, EEE)\n\
        \CCC = (ZZZ, GGG)\n\
        \DDD = (DDD, DDD)\n\
        \EEE = (EEE, EEE)\n\
        \GGG = (GGG, GGG)\n\
        \ZZZ = (ZZZ, ZZZ)"

testInput2 :: DesertMap
testInput2 =
    parseInput
        "LLR\n\
        \\n\
        \AAA = (BBB, BBB)\n\
        \BBB = (AAA, ZZZ)\n\
        \ZZZ = (ZZZ, ZZZ)"

eighthDecemberSolution1 :: IO Int
eighthDecemberSolution1 = fst . solution1 (== "ZZZ") "AAA" <$> input

testInput3 :: DesertMap
testInput3 =
    parseInput
        "LR\n\
        \\n\
        \11A = (11B, XXX)\n\
        \11B = (XXX, 11Z)\n\
        \11Z = (11B, XXX)\n\
        \22A = (22B, XXX)\n\
        \22B = (22C, 22C)\n\
        \22C = (22Z, 22Z)\n\
        \22Z = (22B, 22B)\n\
        \XXX = (XXX, XXX)"

findRecurrence :: String -> DesertMap -> Int
findRecurrence x dm =
    let n = unsafeSearchNode x (nodes dm)
        recur = (\(i, _, _) -> i) . head . tail $ iterate loopSearch (0, n, dm)
     in recur

loopSearch :: (Int, Node, DesertMap) -> (Int, Node, DesertMap)
loopSearch (i, x, dm)
    | ((/= 'Z') . last . nId) x = go (nId x) dm
    | otherwise = let (i', x', dm') = go nextNode nextDM in (i' + 1, x', dm')
  where
    nextMove = (head . moves) dm
    nextDM = (dm{moves = (tail . moves) dm})
    nextNode = fromJust $ mapStep (const False) nextMove x
    go :: String -> DesertMap -> (Int, Node, DesertMap)
    go n m =
        let (i', x') = solution1 ((== 'Z') . last) n m
            dm' = m{moves = (drop i' . moves) m}
         in (i', x', dm')

solution2 :: DesertMap -> Int
solution2 dm = foldl1 lcm $ fmap (\n -> findRecurrence (nId n) dm) startingPoints
  where
    startingPoints = (elems . M.filter ((== 'A') . last . nId) . nodes) dm

eighthDecemberSolution2 :: IO Int
eighthDecemberSolution2 = solution2 <$> input
