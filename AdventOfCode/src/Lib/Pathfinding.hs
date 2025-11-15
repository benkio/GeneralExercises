module Lib.Pathfinding (Node (..), mapToPaths, pathToCoord, minimumSteps, defaultMapToPaths) where

import Data.Functor ((<&>))
import Data.IORef
import Data.List (minimumBy, sortOn)
import Data.Map (Map, alter, elems, empty, size, (!?))
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Tree (Tree, unfoldTreeM)
import Debug.Trace
import Lib.Coord (Coord, manhattanDistance)
import Lib.CoordMap (findBranches, findBranchesFull, findCardinalNeighboors, updateLowestScore)
import Lib.Direction (Direction (..))
import Text.Printf (printf)

data Node a = N {nc :: Coord, val :: a, distanceFromParent :: Int, turnL :: Int, turnR :: Int, direction :: Direction}

instance (Show a) => Show (Node a) where
    show (N{nc = c, val = v, distanceFromParent = d, turnL = tl, turnR = tr}) =
        printf "%s - %s - %d - < %d - > %d" (show c) (show v) d tl tr

mapToPaths ::
    (Show a) =>
    (Coord, a) ->
    Direction ->
    (Coord -> a -> Bool) ->
    (Node a -> Int) ->
    (Int -> Int -> Bool) ->
    (Int -> Int -> Bool) ->
    ((Node a, Int) -> Int) ->
    Map Coord a ->
    [[Node a]]
mapToPaths (sc, v) direction extraNodeF scoreNodeF discardNodeByScoreF keepNextNodeByScoreF sortNodesF ms = go empty [] [start]
  where
    start = (N{nc = sc, val = v, distanceFromParent = 0, turnL = 0, turnR = 0, direction = direction}, 0, [])
    go _ result [] = result
    go visitedScoreMap result (x : xs)
        | extraNodeF c elem =
            -- trace ("END: " ++ show (currentTot)) $
            go visitedScoreMap' (prev' : result) xs
        | endCheck c = go visitedScoreMap' result xs
        | otherwise =
            -- trace (printf "r %s %d %d" (show c) (length xs) (currentTot)) $
            go visitedScoreMap' result branches
      where
        (node@N{nc = c, val = elem, distanceFromParent = dis, turnL = tl, turnR = tr, direction = dir}, tot, prev) = x
        endCheck x = maybe False (discardNodeByScoreF currentTot) (visitedScoreMap !? x)
        currentTot = tot + scoreNodeF node
        visitedScoreMap' = updateLowestScore c currentTot visitedScoreMap
        prev' = prev ++ [node]
        filterNext =
            filter
                ( \(n', tot, _) ->
                    (nc n' /= c && maybe True (keepNextNodeByScoreF tot) (visitedScoreMap' !? nc n'))
                        || (nc n' == c && keepNextNodeByScoreF tot currentTot)
                )
        sortByDistanceToTarget =
            sortOn (\(n, tot, _) -> sortNodesF (n, tot))
        branches =
            sortByDistanceToTarget
                . filterNext
                . (xs ++)
                $ searchTreeBranches c dir currentTot extraNodeF prev' ms

searchTreeBranches ::
    Coord ->
    Direction ->
    Int ->
    (Coord -> a -> Bool) ->
    [Node a] ->
    Map Coord a ->
    [(Node a, Int, [Node a])]
searchTreeBranches c dir tot extraNodeF prev ms =
    mapMaybe
        ( \(c', d, turns, dir') ->
            ms !? c' <&> \x ->
                (N{nc = c', val = x, distanceFromParent = d, turnL = fst turns, turnR = snd turns, direction = dir'}, tot, prev)
        )
        $ findBranches c dir extraNodeF ms

pathToCoord :: [Node a] -> Map Coord a -> (Coord -> a -> Bool) -> [Coord]
pathToCoord ns ms extraNodeF = ((nc . head) ns :) . foldl foldNodes [] $ zip ns (tail ns)
  where
    branch n n' =
        head
            . filter (\cs -> ((== nc n') . last) cs && length cs == distanceFromParent n')
            . fmap (fmap (\(c, _, _, _) -> c))
            $ findBranchesFull (nc n) (direction n) extraNodeF ms
    foldNodes acc (n, n') =
        acc ++ branch n n'

minimumSteps :: [[Node a]] -> (Int, [Node a])
minimumSteps = minimumBy (comparing fst) . fmap (\ns -> ((sum . fmap distanceFromParent) ns, ns))

defaultMapToPaths :: (Show a) => (Coord, a) -> Coord -> Map Coord a -> [[Node a]]
defaultMapToPaths start endCoord = mapToPaths start East endCondition scoreF discardNodeByScoreF keepNextNodeByScoreF sortNodesF
  where
    endCondition c _ = c == endCoord
    scoreF = uncurry (+) . manhattanDistance endCoord . nc
    discardNodeByScoreF currScore prevScore = currScore < prevScore
    keepNextNodeByScoreF nextScore currScore = nextScore < currScore
    sortNodesF (node, score) = score
