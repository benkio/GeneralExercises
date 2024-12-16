{-# LANGUAGE TupleSections #-}

module Lib.Pathfinding (Node (..), mapToTree) where

import Text.Printf (printf)

import Data.Functor ((<&>))
import Data.Map (Map, (!?))
import Data.Maybe (mapMaybe)
import Lib.Coord (Coord, findCardinalNeighboors)
import Lib.CoordMap (findBranches)
import Lib.Direction (Direction)

import Data.Tree (Tree, unfoldTree)

data Node a = N {nc :: Coord, val :: a, distanceFromParent :: Int, turnL :: Int, turnR :: Int}

instance (Show a) => Show (Node a) where
    show (N{nc = c, val = v, distanceFromParent = d, turnL = tl, turnR = tr}) =
        printf "%s - %s - %d - < %d - > %d" (show c) (show v) d tl tr

mapToTree :: (Coord, a) -> Direction -> (Coord -> a -> Bool) -> Map Coord a -> Tree (Node a)
mapToTree (sc, v) direction extraNodeF ms =
    unfoldTree (generateTree) (sc, 0, v, (0, 0), direction, [])
  where
    generateTree (c, dis, elem, (tl, tr), dir, visited) =
        if extraNodeF c elem
            then (N{nc = c, val = elem, distanceFromParent = dis, turnL = tl, turnR = tr}, [])
            else
                ( N{nc = c, val = elem, distanceFromParent = dis, turnL = tl, turnR = tr}
                , generateTreeBranches ms c visited (searchTreeBranches c dir extraNodeF ms)
                )

searchTreeBranches ::
    Coord ->
    Direction ->
    (Coord -> a -> Bool) ->
    Map Coord a ->
    [(Coord, Int, a, (Int, Int), Direction)]
searchTreeBranches c dir extraNodeF ms =
    mapMaybe (\(c', d, turns, dir') -> ms !? c' <&> \x -> (c', d, x, turns, dir')) $ findBranches c dir extraNodeF ms

generateTreeBranches ::
    Map Coord a ->
    Coord ->
    [Coord] ->
    [(Coord, Int, a, (Int, Int), Direction)] ->
    [(Coord, Int, a, (Int, Int), Direction, [Coord])]
generateTreeBranches ms p visited cs =
    fmap
        ( \(c, dist, a, turns, dir) -> (c, dist, a, turns, dir, (p : visited) ++ fmap extractCoord notVisitedNext)
        )
        notVisitedNext
  where
    extractCoord = (\(c, _, _, _, _) -> c)
    notVisitedNext = filter (\(c, _, _, _, _) -> c `notElem` visited) $ cs
