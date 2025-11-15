module TwentyTwentyFour.December12 where

import Data.Bifunctor (first)
import Data.List (groupBy, nub, sortOn)
import Data.Map (
    Map,
    empty,
    filterWithKey,
    findMin,
    foldrWithKey,
    fromList,
    insert,
    member,
    notMember,
    singleton,
    size,
    toList,
    union,
    (!?),
 )
import qualified Data.Map as M (filter, map, null)
import Data.Maybe (isJust)
import Lib.Coord (Coord, cardinalNeighboors)
import Lib.CoordMap (findCardinalNeighboors)
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
                    Left ((x, y), v)
            )

-- Slow
regions :: Farm -> [Farm]
regions f =
    concatMap
        (splitInRegion . (\x -> M.filter (== x) f))
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
                    else first (`union` insert c v region) (findRegion (c, v) rest')
            )
            (empty, rest)
            neighbours
  where
    neighbours = findCardinalNeighboors c ms
    rest = filterWithKey (\k _ -> k `notMember` neighbours) ms

farmPrice :: Farm -> Farm -> Int
farmPrice f ms =
    size ms * length (perimeter f ms)

perimeter :: Farm -> Farm -> [Coord]
perimeter f = foldrWithKey (\c v acc -> acc ++ perimeterSingle c v f) []

perimeterSingle :: Coord -> Crop -> Farm -> [Coord]
perimeterSingle c v f = fmap fst . toList $ neighbours
  where
    ns = findCardinalNeighboors c f
    filler = foldl (\acc x -> insert x '.' acc) empty $ cardinalNeighboors c
    neighbours = M.filter (/= v) (ns `union` filler)

solution1 :: Farm -> Int
solution1 f = sum . fmap (farmPrice f) $ regions f

december12Solution1 :: IO Int
december12Solution1 = solution1 <$> input

countPerimeterSize :: Farm -> [Coord] -> Int
countPerimeterSize _ [] = 0
countPerimeterSize f (c : cs) =
    let (as, cs') = findAngle f c cs in length as + countPerimeterSize f cs'

findAngle :: Farm -> Coord -> [Coord] -> ([Coord], [Coord])
findAngle f c cs = (otherAngles ++ himself, cs')
  where
    himself =
        let xs = filter (\x -> x == c && selfAngleCheck x) cs
         in if length xs == 3 then c : xs else xs
    otherAngles = nub $ filter (isAngle c) cs
    cs' = filter (/= c) cs
    isAngle (x, y) c =
        c == (x - 1, y - 1) && (isJust (f !? (x - 1, y)) /= isJust (f !? (x, y - 1)))
            || c == (x - 1, y + 1) && (isJust (f !? (x - 1, y)) /= isJust (f !? (x, y + 1)))
            || c == (x + 1, y - 1) && (isJust (f !? (x + 1, y)) /= isJust (f !? (x, y - 1)))
            || c == (x + 1, y + 1) && (isJust (f !? (x + 1, y)) /= isJust (f !? (x, y + 1)))
    selfAngleCheck (x, y) =
        (isJust (f !? (x - 1, y)) && isJust (f !? (x, y - 1)))
            || (isJust (f !? (x, y - 1)) && isJust (f !? (x + 1, y)))
            || (isJust (f !? (x + 1, y)) && isJust (f !? (x, y + 1)))
            || (isJust (f !? (x, y + 1)) && isJust (f !? (x - 1, y)))

farmPrice' :: Farm -> Farm -> Int
farmPrice' f ms =
    if M.null ms
        then 0
        else size ms * countPerimeterSize ms (perimeter f ms)

solution2 :: Farm -> Int
solution2 f = sum . fmap (farmPrice' f) $ regions f

-- too high 790543
-- too low  783428
-- too low  780424
december12Solution2 :: IO Int
december12Solution2 = solution2 <$> input

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

testInput'' :: Farm
testInput'' =
    parseInput
        "EEEEE\n\
        \EXXXX\n\
        \EEEEE\n\
        \EXXXX\n\
        \EEEEE"

testInput''' :: Farm
testInput''' =
    parseInput
        "AAAAAA\n\
        \AAABBA\n\
        \AAABBA\n\
        \ABBAAA\n\
        \ABBAAA\n\
        \AAAAAA"

testInput'''' :: Farm
testInput'''' =
    parseInput
        "OOOOO\n\
        \OXOXO\n\
        \OOOOO\n\
        \OXOXO\n\
        \OOOOO"
