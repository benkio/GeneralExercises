module TwentyTwentyOne.TwentyFifthDecember where

import Data.Map (Map)
import qualified Data.Map as M (filterWithKey, findMax, fromList, lookup, map, mapKeys, union)
import Data.Maybe (fromJust, maybe)

type Coord = (Int, Int)

type Floor = Map Coord FloorSpace

data SeaCucumber = E | S deriving (Eq)

data FloorSpace = Empty | Occupied SeaCucumber deriving (Eq)

instance Show FloorSpace where
  show Empty = "."
  show (Occupied x) = show x

instance Show SeaCucumber where
  show E = ">"
  show S = "v"

parseInput :: String -> Map Coord FloorSpace
parseInput =
  M.fromList
    . concatMap
      ( \(y, l) ->
          ( fmap (\(x, c) -> ((x, y), parseFloorSpace c))
              . zip [0 ..]
          )
            l
      )
    . zip [0 ..]
    . lines

parseFloorSpace :: Char -> FloorSpace
parseFloorSpace = maybe Empty Occupied . parseCucumber

parseCucumber :: Char -> Maybe SeaCucumber
parseCucumber '>' = Just E
parseCucumber 'v' = Just S
parseCucumber _ = Nothing

isEastSeacucumber :: SeaCucumber -> Bool
isEastSeacucumber E = True
isEastSeacucumber _ = False

isSouthSeacucumber :: SeaCucumber -> Bool
isSouthSeacucumber S = True
isSouthSeacucumber _ = False

seacucumberNextPos :: SeaCucumber -> Coord -> Floor -> Coord
seacucumberNextPos s (x, y) m
  | isEastSeacucumber s && x == maxX = (0, y)
  | isEastSeacucumber s && x /= maxX = (x + 1, y)
  | isSouthSeacucumber s && y == maxY = (x, 0)
  | isSouthSeacucumber s && y /= maxY = (x, y + 1)
  where
    ((maxX, maxY), _) = M.findMax m

isOccupied :: Coord -> Floor -> Bool
isOccupied c m = (checkFloorSpace . fromJust) $ M.lookup c m
  where
    checkFloorSpace (Occupied _) = True
    checkFloorSpace _ = False

findMovingSeacucumber :: SeaCucumber -> Floor -> Coord -> FloorSpace -> Bool
findMovingSeacucumber target m c (Occupied s) = s == target && not (isOccupied (seacucumberNextPos s c m) m)
findMovingSeacucumber _ _ _ _ = False

moveSeacucumber :: SeaCucumber -> Floor -> Floor
moveSeacucumber s m =
  let movingSeacucumber = M.filterWithKey (findMovingSeacucumber s m) m
      movedSeacucumber = M.mapKeys (\c -> seacucumberNextPos s c m) movingSeacucumber `M.union` M.map (const Empty) movingSeacucumber
   in movedSeacucumber `M.union` m

moveFloor :: Floor -> Floor
moveFloor = moveSeacucumber S . moveSeacucumber E

floorEvolution :: Floor -> [Floor]
floorEvolution = iterate moveFloor

solution1 :: Floor -> Int
solution1 = (\l -> stopWhenEqual (head l) (tail l)) . zip [0 ..] . floorEvolution
  where
    stopWhenEqual (i, x) ((i', y) : ys)
      | x == y = i'
      | otherwise = stopWhenEqual (i', y) ys

inputTest :: String
inputTest =
  "v...>>.vv>\n\
  \.vv>>.vv..\n\
  \>>.>v>...v\n\
  \>>v>>.>.v.\n\
  \v>v.vv.v..\n\
  \>.>>..v...\n\
  \.vv..>.>v.\n\
  \v.v..>>v.v\n\
  \....v..v.>"

inputTest' :: String
inputTest' =
  "....>.>v.>\n\
  \v.v>.>v.v.\n\
  \>v>>..>v..\n\
  \>>v>v>.>.v\n\
  \.>v.v...v.\n\
  \v>>.>vvv..\n\
  \..v...>>..\n\
  \vv...>>vv.\n\
  \>.v.v..v.v"

input :: IO String
input = readFile "input/2021/25December.txt"

twentyFifthDecemberSolution1 :: IO Int
twentyFifthDecemberSolution1 = solution1 . parseInput <$> input

twentyFifthDecemberSolution2 :: IO Int
twentyFifthDecemberSolution2 = undefined
