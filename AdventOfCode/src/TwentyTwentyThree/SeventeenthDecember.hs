{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.SeventeenthDecember where

import Debug.Trace (traceShow, traceShowId)

import Data.Maybe (fromJust, mapMaybe)

import Data.Set (Set)
import Data.List (delete, sortBy)
import Data.Map (Map, fromList)
import Data.Map as Map (adjust, lookup, insert, member)

import Data.Set as Set (empty, notMember, insert)


newtype HeatLossMap = HLM (Map (Int, Int) Int) deriving (Show)
newtype NodeWeight = NW (Map (Int, Int) Int) deriving (Show)
data Direction = L | R | U | D deriving (Eq, Show)

input :: IO HeatLossMap
input = parseInput <$> readFile "input/2023/17December.txt"

solution1 hlm = loop (Set.empty) (initialNodeWeigth hlm) hlm [((0,0), [])]

seventeenthDecemberSolution1 :: IO Int
seventeenthDecemberSolution1 = undefined

solution2 = undefined

seventeenthDecemberSolution2 :: IO Int
seventeenthDecemberSolution2 = undefined

initialNodeWeigth :: HeatLossMap -> NodeWeight
initialNodeWeigth (HLM m) = (NW . Map.insert (0, 0) 0 . fmap (const (maxBound :: Int))) m

parseInput :: String -> HeatLossMap
parseInput input = HLM $ fromList $ concat $ zipWith (\rowIndex row -> parseRow rowIndex row) [0 ..] (lines input)
  where
    parseRow :: Int -> String -> [((Int, Int), Int)]
    parseRow rowIndex row = zipWith (\colIndex char -> ((colIndex, rowIndex), read [char])) [0 ..] row

testInput :: HeatLossMap
testInput =
    parseInput
        "2413432311323\n\
        \3215453535623\n\
        \3255245654254\n\
        \3446585845452\n\
        \4546657867536\n\
        \1438598798454\n\
        \4457876987766\n\
        \3637877979653\n\
        \4654967986887\n\
        \4564679986453\n\
        \1224686865563\n\
        \2546548887735\n\
        \4322674655533"

loop :: Set (Int,Int) -> NodeWeight -> HeatLossMap -> [((Int, Int), [Direction])] -> NodeWeight
loop _ nw _ [] = nw
loop vs nw hlm ((c, p):xs) = loop vs' newNodeWeight hlm (nn ++ xs)
  where
    nnw = traceShowId $ nextNodes hlm vs c p
    newNodeWeight = updateWeight (fmap fst nnw) nw hlm c
    vs' = Set.insert c vs
    nnwSorted = sortBy (\(c, _) (c', _) -> sortNodesByWeight nw c c') nnw
    nn = fmap (\(c, d) ->  (c, p ++ [d])) nnwSorted

sortNodesByWeight :: NodeWeight -> (Int,Int) -> (Int,Int) -> Ordering
sortNodesByWeight (NW nw) c c' = nwc `compare` nwc'
  where
    nwc  = fromJust $ Map.lookup (traceShowId c) nw
    nwc' = fromJust $ Map.lookup (traceShowId c') nw

updateWeight :: [(Int,Int)] -> NodeWeight -> HeatLossMap -> (Int, Int) -> NodeWeight
updateWeight nn (NW nwm) (HLM hlm) c = NW $ foldl (\m' n -> Map.adjust (\w -> min w (cw + (nv n))) n m') nwm nn
  where
    cw = fromJust $ Map.lookup c nwm
    nv n = fromJust $ Map.lookup n hlm

nextNodes :: HeatLossMap -> Set (Int,Int) -> (Int, Int) -> [Direction] -> [((Int, Int), Direction)]
nextNodes (HLM hlm) vs c ds = (filter ((\x -> Set.notMember x vs && Map.member x hlm) . fst) .  nextNodesSingle c) $ availableDirections ds

nextNodesSingle :: (Int, Int) -> [Direction] -> [((Int, Int), Direction)]
nextNodesSingle (x, y) = fmap move
  where
    move L = ((x - 1, y), L)
    move R = ((x + 1, y), R)
    move U = ((x, y - 1), U)
    move D = ((x, y + 1), D)

last3InARow :: [Direction] -> Maybe Direction
last3InARow ds
    | length last3 < 3 || any (/= head last3) last3 = Nothing
    | otherwise = Just $ head last3
  where
    last3 = (take 3 . reverse) ds

availableDirections :: [Direction] -> [Direction]
availableDirections ds = maybe (allDirections) (`delete` allDirections) mayForbiddenDirection
  where
    mayForbiddenDirection = last3InARow ds
    allDirections = [L, R, U, D]
