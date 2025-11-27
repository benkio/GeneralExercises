module TwentyTwentyThree.December25 where

import Data.Bifunctor (bimap, first, second)
import Data.List.Split (splitOn)
import Data.Map (Map, delete, elems, empty, fromList, insert, insertWith, keys, size, toList, (!))
import qualified Data.Map as M (lookup, map)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace, traceShowId)
import System.Random (randomRIO)
import Text.Printf (printf)

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
    let ns = toList cs
    (node1K, node1V) <- (ns !!) <$> randomRIO (0, length ns - 1)
    (node2K, node2V) <- (\k -> (k, fromMaybe (error ("can't find key: " ++ k ++ " - " ++ show cs)) (M.lookup k cs))) . (node1V !!) <$> randomRIO (0, length node1V - 1)
    let node1K' = node1K ++ ";" ++ node2K
        cs' = (M.map (fmap (\x -> if x == node2K || x == node1K then node1K' else x)) . delete node1K . delete node2K) cs
        node1V' = filter (`notElem` [node1K, node2K]) (node1V ++ node2V)
    return $ insert node1K' node1V' cs'

karger :: Connections -> IO [[String]]
karger cs
    | size cs > 2 = mergeNodes cs >>= karger
    | otherwise = return $ elems cs

-- -- https://en.wikipedia.org/wiki/Karger%27s_algorithm
solution1 cs = do
    [n1, n2] <- karger cs
    if length n1 == 3 && length n2 == 3
        then return $ calculateNodes n1 * calculateNodes n2
        else trace (printf "debug: %d %d" (length n1) (length n2)) $ solution1 cs
  where
    calculateNodes = length . splitOn ";" . head

twentyfifthDecemberSolution1 :: IO Int
twentyfifthDecemberSolution1 = solution1 =<< input

solution2 = undefined

twentyfifthDecemberSolution2 :: IO Int
twentyfifthDecemberSolution2 = undefined
