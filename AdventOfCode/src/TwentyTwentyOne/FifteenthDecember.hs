module TwentyTwentyOne.FifteenthDecember where

import Data.List (groupBy, minimumBy, sort)
import Data.List.Split (chunksOf)
import Data.Map (Map, (!))
import qualified Data.Map as M (findMax, fromList, insert, keys, lookup)
import Data.Maybe (mapMaybe)
import Debug.Trace

type Coord = (Int, Int)

parseInput :: String -> Map Coord Int
parseInput = M.fromList . concatMap (\(y, l) -> (fmap (\(x, c) -> ((x, y), read [c] :: Int)) . zip [0 ..]) l) . zip [0 ..] . lines

neighboorsCoords :: Coord -> [Coord]
neighboorsCoords (x, y) =
  [(a, b) | a <- [(x -1) .. (x + 1)], b <- [(y -1) .. (y + 1)], (a == x || b == y) && not (a == x && b == y)]

neighboorsWeighted :: Map Coord Int -> (Coord, Int) -> [(Coord, Int)]
neighboorsWeighted m (c, vc) = mapMaybe (\nc -> fmap (\nvc -> (nc, vc + nvc)) (M.lookup nc m)) (neighboorsCoords c)

buildWeightedMap :: Map Coord Int -> [(Coord, Int)] -> Map Coord Int -> Map Coord Int
buildWeightedMap v [] _ = v
buildWeightedMap v es m =
  let -- neighboors(ns) of all es filtered not member of v or minimum
      -- foreach ns calc the weight based on the element of es
      ns = (filter (\(c, x) -> foldl (\_ x' -> x < x') True (M.lookup c v)) . concatMap (neighboorsWeighted m)) es
      -- sort ns -> group -> minimum
      ns' = (fmap (minimumBy (\(_, y) (_, y') -> y `compare` y')) . groupBy (\(c, _) (c', _) -> c == c') . sort) ns
      -- add them to v
      v' = foldl (\acc (c, y) -> M.insert c y acc) v ns'
      -- recurse with the new edge and v'
   in buildWeightedMap v' ns' m

--step v ns m = iterate (\(x, y) -> buildWeightedMap x y m) (v,ns)

solution1 :: String -> Int
solution1 =
  (\x -> min (x ! (0, 1)) (x ! (1, 0)))
    . (\m -> buildWeightedMap (M.fromList [M.findMax m]) [M.findMax m] m)
    . parseInput

inputTest :: String
inputTest =
  "1163751742\n\
  \1381373672\n\
  \2136511328\n\
  \3694931569\n\
  \7463417111\n\
  \1319128137\n\
  \1359912421\n\
  \3125421639\n\
  \1293138521\n\
  \2311944581\n"

printMatrix :: Map (Int, Int) Int -> IO ()
printMatrix m =
  let maxX = (maximum . fmap fst . M.keys) m
      maxY = (maximum . fmap snd . M.keys) m
      grid = [v | y <- [0 .. maxY], x <- [0 .. maxX], v <- " " ++ show (m ! (x, y))]
      gridByLine = chunksOf (maxX + 1) grid
   in mapM_ putStrLn gridByLine

input :: IO String
input = readFile "input/2021/15December.txt"

fifteenthDecemberSolution1 :: IO Int
fifteenthDecemberSolution1 = solution1 <$> input

fifteenthDecemberSolution2 :: IO Int
fifteenthDecemberSolution2 = undefined
