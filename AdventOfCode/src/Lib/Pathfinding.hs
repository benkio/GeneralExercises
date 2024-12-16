{-# LANGUAGE TupleSections #-}

module Lib.Pathfinding (Node (..), mapToPaths) where

import Text.Printf (printf)

import Data.Functor ((<&>))
import Data.IORef
import Data.Map (Map, alter, empty, size, (!?))
import Data.Maybe (mapMaybe)
import Debug.Trace
import Lib.Coord (Coord, findCardinalNeighboors)
import Lib.CoordMap (findBranches)
import Lib.Direction (Direction)
import Data.List (sortBy)

import Data.Tree (Tree, unfoldTreeM)

data Node a = N {nc :: Coord, val :: a, distanceFromParent :: Int, turnL :: Int, turnR :: Int}

instance (Show a) => Show (Node a) where
    show (N{nc = c, val = v, distanceFromParent = d, turnL = tl, turnR = tr}) =
        printf "%s - %s - %d - < %d - > %d" (show c) (show v) d tl tr

updateLowestScore :: Coord -> Int -> Map Coord Int -> Map Coord Int
updateLowestScore c v =
    alter (\mv -> maybe (Just v) (Just . min v) mv) c

mapToPaths ::
    (Coord, a) ->
    Direction ->
    (Coord -> Int) ->
    (Coord -> a -> Bool) ->
    (Node a -> Int) ->
    Map Coord a ->
    [[(Node a, Int)]]
mapToPaths (sc, v) direction distanceToTargetF extraNodeF scoreNodeF ms = go empty [] [start]
  where
    start = (sc, 0, v, (0, 0), direction, 0, [])
    go _ result [] = result
    go visitedScoreMap result (x : xs)
        | extraNodeF c elem = trace ("END: " ++ show (currentTot)) $ go visitedScoreMap' (prev' : result) xs'
        | endCheck c = go visitedScoreMap' result xs'
        | otherwise = trace ("r " ++ (show c) ++ " " ++ show (length xs') ++ " " ++ show (currentTot)) $ go visitedScoreMap' result branches
      where
        (c, dis, elem, (tl, tr), dir, tot, prev) = x
        endCheck x = maybe False (\prevTot -> prevTot < currentTot) (visitedScoreMap !? x)
        node = N{nc = c, val = elem, distanceFromParent = dis, turnL = tl, turnR = tr}
        currentTot = tot + scoreNodeF node
        visitedScoreMap' = updateLowestScore c currentTot visitedScoreMap
        prev' = prev ++ [(node, currentTot)]
        xs' = filterNext xs
        filterNext =
            filter
                ( \(c', _, _, _, _, tot, _) ->
                    (c' /= c && (maybe True (\prevTot -> tot < prevTot) (visitedScoreMap' !? c')))
                        || (c' == c && tot < currentTot)
                )
        sortByDistanceToTarget =
          sortBy ( \(x, _, _, _, _, _, _) (x', _, _, _, _, _, _) -> distanceToTargetF x `compare` distanceToTargetF x')
        branches =
          sortByDistanceToTarget
          .  filterNext . (xs ++) $
                searchTreeBranches c dir currentTot extraNodeF prev' ms

-- branchesFiltered bank = filter (lookAheadFilter bank) branches

searchTreeBranches ::
    Coord ->
    Direction ->
    Int ->
    (Coord -> a -> Bool) ->
    [(Node a, Int)] ->
    Map Coord a ->
    [(Coord, Int, a, (Int, Int), Direction, Int, [(Node a, Int)])]
searchTreeBranches c dir tot extraNodeF prev ms =
    mapMaybe (\(c', d, turns, dir') -> ms !? c' <&> \x -> (c', d, x, turns, dir', tot, prev)) $ findBranches c dir extraNodeF ms
