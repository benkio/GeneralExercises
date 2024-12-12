module TwentyTwentyFour.December12 where

import Data.List (groupBy, sortOn)

import Data.Bifunctor (first)
import Data.Map (Map, empty, filterWithKey, foldrWithKey, fromList, insert, member, notMember, singleton, size, toList, union)
import qualified Data.Map as M (filter, map, null)

import Debug.Trace
import Lib.Coord (Coord, findCardinalNeighboors)
import Lib.List ((\\))
import Lib.Parse (parseGridWithElemSelection)

type Crop = Char
type Farm = Map Coord Crop

input :: IO Farm
input = parseInput <$> readFile "input/2024/December12.txt"

parseInput :: String -> Farm
parseInput =
    fromList
        . fst
        . parseGridWithElemSelection
            ( \y x v ->
                Just $
                    Left $
                        ((x, y), v)
            )

-- Slow
regions :: Farm -> [Farm]
regions f =
    ( concatMap splitInRegion
        . fmap (\x -> M.filter (== x) f)
    )
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

splitInRegion :: Farm -> [Farm]
splitInRegion ms =
    (\(fs, rest) -> fs ++ (fmap (uncurry singleton) . toList) rest) $
        foldrWithKey
            ( \c v (regions, rest) ->
                if any (member c) regions
                    then (regions, rest)
                    else first (: regions) (findRegion (c, v) rest)
            )
            ([], ms)
            ms

findRegion :: (Coord, Crop) -> Farm -> (Farm, Farm)
findRegion (c, v) ms
    | M.null ms = (singleton c v, empty)
    | otherwise =
        foldrWithKey
            ( \c v (region, rest') ->
                if member c region
                    then (region, rest')
                    else first (`union` (insert c v region)) (findRegion (c, v) rest')
            )
            (empty, rest)
            neighbours
  where
    neighbours = findCardinalNeighboors c ms
    rest = filterWithKey (\k _ -> k `notMember` neighbours) ms

testInput :: Farm
testInput =
    parseInput
        "AAAA\n\
        \BBCD\n\
        \BBCC\n\
        \EEEC"

testInput' :: Farm
testInput' =
    parseInput
        "RRRRIICCFF\n\
        \RRRRIICCCF\n\
        \VVRRRCCFFF\n\
        \VVRCCCJFFF\n\
        \VVVVCJJCFE\n\
        \VVIVCCJJEE\n\
        \VVIIICJJEE\n\
        \MIIIIIJJEE\n\
        \MIIISIJEEE\n\
        \MMMISSJEEE"

farmPrice :: Farm -> Farm -> Int
farmPrice f ms =
    size ms * perimeter f ms

perimeter :: Farm -> Farm -> Int
perimeter f ms = foldrWithKey (\c v acc -> acc + perimeterSingle c v f) 0 ms

perimeterSingle :: Coord -> Crop -> Farm -> Int
perimeterSingle c v f =
    if size ns == 4
        then neighbours
        else (4 - size ns) + neighbours
  where
    ns = findCardinalNeighboors c f
    neighbours = (size . M.filter (/= v)) ns

solution1 :: Farm -> Int
solution1 f = sum . fmap (farmPrice f) $ regions f

december12Solution1 :: IO Int
december12Solution1 = solution1 <$> input

solution2 :: Farm -> Int
solution2 = undefined

december12Solution2 :: IO Int
december12Solution2 = solution2 <$> input
