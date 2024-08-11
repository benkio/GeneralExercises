module TwentyTwentyThree.TwentyFifthDecember where

import Data.Maybe (fromJust)
import Data.Map (Map, insertWith, empty, keys, fromList)
import qualified Data.Map as M (lookup)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap, first, second)

type Connections = Map String [String]

input :: IO Connections
input = parseInput <$> readFile "input/2023/25December.txt"

parseInput :: String -> Connections
parseInput = foldl (\m (x, v) -> insertWith (++) x [v] m) empty .
  concatMap parseLineWire . lines
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

unsafeLookup :: Connections -> String -> [String]
unsafeLookup m s = fromJust $ M.lookup s m

-- if empty is connected, otherwise returns the ones not connected (left)
isConnected :: Connections -> [String]
isConnected cs = go (keys cs) [(head . keys) cs]
  where
    go :: [String] -> [String] -> [String]
    go [] _ = []
    go xs [] = xs
    go xs ss =
      let
        leftConnections = filter (`notElem` ss) xs
      in go leftConnections (concatMap (filter (`elem` leftConnections) . unsafeLookup cs) ss)

-- https://en.wikipedia.org/wiki/Karger%27s_algorithm
solution1 = undefined

twentyfifthDecemberSolution1 :: IO Int
twentyfifthDecemberSolution1 = undefined

solution2 = undefined

twentyfifthDecemberSolution2 :: IO Int
twentyfifthDecemberSolution2 = undefined

-- works
test :: [String]
test = isConnected $ fromList [("bvb",["ntq","hfx","xhk","rhn"]),("cmg",["rzs","lhk","nvd","qnr"]),("frs",["lsr","lhk","qnr","rsh"]),("hfx",["ntq","bvb","rhn","xhk"]),("jqt",["ntq","xhk","rhn"]),("lhk",["frs","lsr","nvd","cmg"]),("lsr",["frs","rzs","lhk","pzl","rsh"]),("ntq",["xhk","bvb","hfx","jqt"]),("nvd",["lhk","qnr","pzl","cmg"]),("pzl",["nvd","lsr","rsh"]),("qnr",["frs","rzs","nvd","cmg"]),("rhn",["hfx","bvb","xhk","jqt"]),("rsh",["rzs","lsr","pzl","frs"]),("rzs",["rsh","lsr","cmg","qnr"]),("xhk",["ntq","bvb","rhn","hfx","jqt"])]
