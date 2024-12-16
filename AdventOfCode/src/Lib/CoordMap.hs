module Lib.CoordMap (findBranches) where

import Data.Bifunctor (first, bimap)
import Data.Map (Map, keys, toList, (!?))
import qualified Data.Map as M (filterWithKey)
import Data.Maybe (listToMaybe, mapMaybe)
import Lib.Coord (Coord, findCardinalNeighboors)
import Lib.CoordDirection (changeDirection)
import Lib.Direction (Direction)

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
findBranches c d extraNodeF ms = mapMaybe (\x -> go [c] ((0,0),d) 1 x) cNeighboors
  where
    splitTurns t = if t < 0 then (abs t, 0) else (0, t)
    calcTurns dir current target = (first splitTurns (changeDirection dir current target))
    cNeighboors = keys $ findCardinalNeighboors c ms
    go :: [Coord] -> ((Int, Int), Direction) -> Int -> Coord -> Maybe (Coord, Int, (Int, Int), Direction)
    go visited ((tl,tr), direction) distance x =
        let
            next = M.filterWithKey (\k _ -> k `notElem` visited) $ findCardinalNeighboors x ms
            extraCondition = maybe False (extraNodeF x) (ms !? x)
            maybeHead = listToMaybe (toList next)
            calcTurnsNext c' = first (bimap (+tl) (+tr)) $ calcTurns direction x c'
         in
            if length next > 1 || extraCondition
                then Just (x, distance, (tl,tr), direction)
                else (\nc -> go (x : visited) (calcTurnsNext nc) (distance + 1) nc) . fst =<< maybeHead
