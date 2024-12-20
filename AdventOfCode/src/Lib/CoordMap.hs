{-# LANGUAGE TupleSections #-}

module Lib.CoordMap (
    findBranches,
    findBranchesFull,
    updateLowestScore,
    findCardinalNeighboors,
    findOrdinalNeighboors,
    findBridges,
    notKeys,
) where

import Data.Bifunctor (bimap, first)
import Data.List (maximumBy)
import Data.Map (Map, alter, fromList, keys, toList, (!?), notMember)
import qualified Data.Map as M (filterWithKey)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Debug.Trace
import Lib.Coord (Coord, cardinalNeighboors, onTheSameLine, ordinalNeighboors, manhattanPath, manhattanDistance)
import Lib.CoordDirection (changeDirection)
import Lib.Direction (Direction)
import Lib.List (pairsWith, pairs)

{-
  Given a:
    - starting point
    - a direction
    - a function to mark a specific point as branch/node
    - a map

  Returns a list of pairs:
    - position of the next branch (where you can take multiple directions)
    - the distance from the starting coord
    - the number of turns taken to reach it
    - the final direction
-}
findBranches ::
    Coord ->
    Direction ->
    (Coord -> a -> Bool) ->
    Map Coord a ->
    [(Coord, Int, (Int, Int), Direction)]
findBranches c d extraNodeF ms = mapMaybe (\x -> go [c] ((0, 0), d) 1 x) cNeighboors
  where
    splitTurns t = if t < 0 then (abs t, 0) else (0, t)
    calcTurns dir current target = (first splitTurns (changeDirection dir current target))
    cNeighboors = keys $ findCardinalNeighboors c ms
    go :: [Coord] -> ((Int, Int), Direction) -> Int -> Coord -> Maybe (Coord, Int, (Int, Int), Direction)
    go visited ((tl, tr), direction) distance x =
        let
            next = M.filterWithKey (\k _ -> k `notElem` visited) $ findCardinalNeighboors x ms
            extraCondition = maybe False (extraNodeF x) (ms !? x)
            maybeHead = listToMaybe (toList next)
            calcTurnsNext c' = first (bimap (+ tl) (+ tr)) $ calcTurns direction x c'
         in
            if length next > 1 || extraCondition
                then Just (x, distance, (tl, tr), direction)
                else (\nc -> go (x : visited) (calcTurnsNext nc) (distance + 1) nc) . fst =<< maybeHead

findBranchesFull ::
    Coord ->
    Direction ->
    (Coord -> a -> Bool) ->
    Map Coord a ->
    [[(Coord, Int, (Int, Int), Direction)]]
findBranchesFull c d extraNodeF ms = mapMaybe (\x -> go [c] ((0, 0), d) 1 x) cNeighboors
  where
    splitTurns t = if t < 0 then (abs t, 0) else (0, t)
    calcTurns dir current target = (first splitTurns (changeDirection dir current target))
    cNeighboors = keys $ findCardinalNeighboors c ms
    go :: [Coord] -> ((Int, Int), Direction) -> Int -> Coord -> Maybe [(Coord, Int, (Int, Int), Direction)]
    go visited ((tl, tr), direction) distance x =
        let
            next = M.filterWithKey (\k _ -> k `notElem` visited) $ findCardinalNeighboors x ms
            extraCondition = maybe False (extraNodeF x) (ms !? x)
            maybeHead = listToMaybe (toList next)
            calcTurnsNext c' = first (bimap (+ tl) (+ tr)) $ calcTurns direction x c'
         in
            if length next > 1 || extraCondition
                then Just [(x, distance, (tl, tr), direction)]
                else
                    ( \nc ->
                        ((x, distance, (tl, tr), direction) :)
                            <$> go (x : visited) (calcTurnsNext nc) (distance + 1) nc
                    )
                        . fst
                        =<< maybeHead

updateLowestScore :: Coord -> Int -> Map Coord Int -> Map Coord Int
updateLowestScore c v =
    alter (\mv -> maybe (Just v) (Just . min v) mv) c

-- Returns non-existing coordinates in the map that:
-- - connect 2 or more existing points
-- - at the given distance
findBridges :: Int -> Map Coord a -> [[Coord]]
findBridges bridgeLength ms = manhattanPaths
  where
    cPairsAtDistance = filter ((\(a,b ) -> a + b == bridgeLength) . uncurry manhattanDistance) . pairs . keys $ ms
    manhattanPaths = filter (\xs -> any (`notMember` ms) xs) . fmap (uncurry manhattanPath) $ cPairsAtDistance

findCardinalNeighboors, findOrdinalNeighboors :: Coord -> Map Coord a -> Map Coord a
findCardinalNeighboors c ms = fromList . mapMaybe (\x -> (x,) <$> ms !? x) $ cardinalNeighboors c
findOrdinalNeighboors c ms = fromList . mapMaybe (\x -> (x,) <$> ms !? x) $ ordinalNeighboors c

notKeys :: Map Coord a -> [Coord]
notKeys ms = [(x,y) | x <- [0..maxY], y <- [0..maxY], isNothing (ms !? (x,y))]
  where
    maxX = fst . maximumBy (comparing fst) . keys $ ms
    maxY = snd . maximumBy (comparing snd) . keys $ ms
