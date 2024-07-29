{-# LANGUAGE LambdaCase #-}

module TwentyTwentyThree.TwentyThirdDecember where

import Data.Map (Map, findMax)
import qualified Data.Map as M (fromList)
import Data.Maybe (catMaybes)

data FieldBlock = Path | SlopeU | SlopeL | SlopeD | SlopeR
type HikeMap = Map (Int, Int) FieldBlock

instance Show FieldBlock where
    show Path = "."
    show SlopeU = "^"
    show SlopeR = ">"
    show SlopeD = "v"
    show SlopeL = "<"

instance Read FieldBlock where
    readsPrec _ = \case
        '.' : xs -> [(Path, xs)]
        '^' : xs -> [(SlopeU, xs)]
        '>' : xs -> [(SlopeR, xs)]
        'v' : xs -> [(SlopeD, xs)]
        '<' : xs -> [(SlopeL, xs)]
        xs -> []

input :: IO HikeMap
input = parseInput <$> readFile "input/2023/23December.txt"

parseInput :: String -> HikeMap
parseInput = M.fromList . concat . zipWith parseLine [0 ..] . lines
  where
    parseLine :: Int -> String -> [((Int, Int), FieldBlock)]
    parseLine y = catMaybes . zipWith (\x c -> if c /= '#' then Just ((x, y), read [c]) else Nothing) [0 ..]

startingPoint :: (Int, Int)
startingPoint = (1, 0)

endingPoint :: HikeMap -> (Int,Int)
endingPoint = fst . findMax

testInput :: HikeMap
testInput =
    parseInput
        "#.#####################\n\
        \#.......#########...###\n\
        \#######.#########.#.###\n\
        \###.....#.>.>.###.#.###\n\
        \###v#####.#v#.###.#.###\n\
        \###.>...#.#.#.....#...#\n\
        \###v###.#.#.#########.#\n\
        \###...#.#.#.......#...#\n\
        \#####.#.#.#######.#.###\n\
        \#.....#.#.#.......#...#\n\
        \#.#####.#.#.#########v#\n\
        \#.#...#...#...###...>.#\n\
        \#.#.#v#######v###.###v#\n\
        \#...#.>.#...>.>.#.###.#\n\
        \#####v#.#.###v#.#.###.#\n\
        \#.....#...#...#.#.#...#\n\
        \#.#########.###.#.#.###\n\
        \#...###...#...#...#.###\n\
        \###.###.#.###v#####v###\n\
        \#...#...#.#.>.>.#.>.###\n\
        \#.###.###.#.###.#.#v###\n\
        \#.....###...###...#...#\n\
        \#####################.#"

solution1 = undefined

twentythirdDecemberSolution1 :: IO Int
twentythirdDecemberSolution1 = undefined

solution2 = undefined

twentythirdDecemberSolution2 :: IO Int
twentythirdDecemberSolution2 = undefined
