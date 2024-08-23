module TwentyTwentyThree.TwentyFifthDecember where

import Debug.Trace (traceShow, traceShowId)

import Data.Bifunctor (bimap, first, second)
import Data.List.Split (splitOn)
import Data.Map (Map, delete, elems, empty, fromList, insert, insertWith, keys, size, toList, (!))
import qualified Data.Map as M (lookup, map)
import Data.Maybe (fromJust)
import System.Random (randomRIO)

type Connections = Map String [String]

input :: IO Connections
input = parseInput <$> readFile "input/2023/25December.txt"

parseInput :: String -> Connections
parseInput =
    foldl (\m (x, v) -> insertWith (++) x [v] m) empty
        . concatMap parseLineWire
        . lines
  where
    parseLineWire :: String -> [(String, String)]
    parseLineWire =
        (\(s, xs) -> concatMap (\x -> [(s, x), (x, s)]) xs) . second (splitOn " " . drop 2) . break (== ':')

testInput :: Connections
testInput =
    parseInput
        "jqt: rhn xhk nvd\n\
        \rsh: frs pzl lsr\n\
        \xhk: hfx\n\
        \cmg: qnr nvd lhk bvb\n\
        \rhn: xhk bvb hfx\n\
        \bvb: xhk hfx\n\
        \pzl: lsr hfx nvd\n\
        \qnr: nvd\n\
        \ntq: jqt hfx bvb xhk\n\
        \nvd: lhk\n\
        \lsr: lhk\n\
        \rzs: qnr cmg lsr rsh\n\
        \frs: qnr lhk lsr"

mergeNodes :: Connections -> IO Connections
mergeNodes cs = do
    let
      ns = toList cs
    (node1K, node1V) <- (ns !!) <$> randomRIO (0, length ns - 1)
    (node2K, node2V) <- (\k -> (k ,cs ! k)) . (node1V !!) <$> randomRIO (0, length node1V - 1)
    let cs' = (M.map (fmap (\x -> if x == node2K then node1K else x)) . delete node1K . delete node2K) cs
        node1V' = filter (`notElem` [node1K, node2K]) (node1V ++ node2V)
    return $ insert node1K node1V' cs'

karger :: Connections -> IO [[String]]
karger cs
    | size cs > 2 = mergeNodes cs >>= karger
    | otherwise = return $ elems cs

-- -- https://en.wikipedia.org/wiki/Karger%27s_algorithm
-- solution1 cs = do
--     [n1, n2] <- karger cs
--     let
--         connections1 = traceShowId $ filter (`elem` fst n2) (snd n1)
--         connections2 = traceShowId $ filter (`elem` fst n1) (snd n2)
--     if length connections2 == 3 && length connections1 == 3
--         then return $ length (fst (traceShowId n1)) * length (fst (traceShowId n2))
--         else solution1 cs

twentyfifthDecemberSolution1 :: IO Int
twentyfifthDecemberSolution1 = undefined

solution2 = undefined

twentyfifthDecemberSolution2 :: IO Int
twentyfifthDecemberSolution2 = undefined
