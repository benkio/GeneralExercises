module TwentyTwentyOne.December09 where

import Data.List (sortOn)
import Data.Map (Map)
import qualified Data.Map as M (fromList, keys, lookup)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Ord
import Data.Set (Set, (\\))
import qualified Data.Set as S (empty, fromList, toList, union)

data Coord = Coord
    { x :: Int
    , y :: Int
    }
    deriving (Show, Ord, Eq)

input :: IO (Map Coord Int)
input = parseInput <$> readFile "input/2021/9December.txt"

inputTest :: Map Coord Int
inputTest =
    parseInput
        "2199943210\n\
        \3987894921\n\
        \9856789892\n\
        \8767896789\n\
        \9899965678"

parseInput :: String -> Map Coord Int
parseInput =
    M.fromList
        . concatMap (\(a, r) -> (fmap (\(b, v) -> (Coord{x = b, y = a}, read [v] :: Int)) . zip [0 ..]) r)
        . zip [0 ..]
        . lines

neighboorsCoords :: Coord -> [Coord]
neighboorsCoords Coord{x = x', y = y'} =
    [Coord{x = a, y = b} | a <- [(x' - 1) .. (x' + 1)], b <- [(y' - 1) .. (y' + 1)], (a == x' || b == y') && (a /= x' || b /= y')]

isLowPoint :: Map Coord Int -> Coord -> Bool
isLowPoint m c = fromMaybe False $ do
    v <- M.lookup c m
    let neighboors = neighboorsCoords c
        nvs = mapMaybe (`M.lookup` m) neighboors
    return $ all (v <) nvs

solution1 :: Map Coord Int -> Int
solution1 m =
    let coords = M.keys m
     in (sum . fmap (\c -> ((+ 1) . fromJust . M.lookup c) m) . filter (isLowPoint m)) coords

december09Solution1 :: IO Int
december09Solution1 = solution1 <$> input

expandPoint :: Map Coord Int -> Coord -> [Coord]
expandPoint m c =
    [x | x <- neighboorsCoords c, maybe False (< 9) (M.lookup x m)]

computeBasin :: Map Coord Int -> Set Coord -> [Coord] -> Set Coord
computeBasin m visited cs =
    let ns = S.fromList (concatMap (expandPoint m) cs) \\ visited
        visited' = S.union ns visited
     in if null ns then visited' else computeBasin m visited' (S.toList ns)

solution2 :: Map Coord Int -> Int
solution2 m =
    let coords = M.keys m
     in (product . take 3 . sortOn Down . fmap (\x -> length (computeBasin m S.empty [x])) . filter (isLowPoint m)) coords

december09Solution2 :: IO Int
december09Solution2 = solution2 <$> input
