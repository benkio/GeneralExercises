{-# LANGUAGE TupleSections #-}

module TwentySixteen.TwentysecondDecember where

import Data.Functor
import Data.Bifunctor
import Data.List
import Data.Map (Map, adjust, fromList, toList)
import qualified Data.Map as Map (insert, lookup, singleton, union)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set (difference, empty, fromList, insert, map, member, singleton, toList, union, unions)
import System.Random
import Text.Printf

type Coordinate = (Int, Int)

type Status = (Coordinate, Grid)

type Grid = Map Coordinate Node

data Node = Node
  { size :: Int,
    used :: Int,
    avail :: Int
  }
  deriving (Eq, Ord)

instance Show Node where
  show Node {size = s, used = u, avail = a} =
    printf "Node: s %d - u %d - a %d" s u a

lookup' :: Coordinate -> Grid -> Node
lookup' c = fromJust . Map.lookup c

input :: IO Grid
input = fromList . fmap parseNode . drop 2 . lines <$> readFile "input/2016/22December.txt"

parseNode :: String -> (Coordinate, Node)
parseNode =
  ( \l ->
      ( nodeNameToCoordinate (head l),
        Node
          { size = ((\x -> read x :: Int) . init) (l !! 1),
            used = ((\x -> read x :: Int) . init) (l !! 2),
            avail = ((\x -> read x :: Int) . init) (l !! 3)
          }
      )
  )
    . words

nodeNameToCoordinate :: String -> (Int, Int)
nodeNameToCoordinate s =
  let x = ((\v -> read v :: Int) . takeWhile ('-' /=) . tail . dropWhile ('x' /=)) s
      y = ((\v -> read v :: Int) . tail . dropWhile ('y' /=)) s
   in (x, y)

solution1 :: [Node] -> [(Node, Node)]
solution1 ns = [(n1, n2) | n1 <- ns, n2 <- ns, n1 /= n2, used n1 /= 0, used n1 <= avail n2]

twentysecondDecemberSolution1 :: IO Int
twentysecondDecemberSolution1 = length . solution1 . fmap snd . toList <$> input

target :: Coordinate
target = (34, 0)

maxX :: Int
maxX = 34

maxY :: Int
maxY = 26

nodeTransfer :: (Node, Node) -> (Node, Node)
nodeTransfer (n1, n2) =
  ( n1 {used = 0, avail = size n1},
    n2 {used = used n2 + used n1, avail = avail n2 - used n1}
  )

possibleTransfers :: Map Coordinate Node -> [(Coordinate, Coordinate)]
possibleTransfers grid =
  let ns = toList grid
   in [ (c1, c2)
        | (c1, n1) <- ns,
          used n1 /= 0,
          (c2, n2) <- ns,
          used n1 <= avail n2,
          isNeighboor c1 c2
      ]

isNeighboor :: Coordinate -> Coordinate -> Bool
isNeighboor (x, y) (x', y') = (x `elem` [x' -1, x' + 1] && y == y') || (y `elem` [y' -1, y' + 1] && x == x')

neighboors :: Coordinate -> [Coordinate]
neighboors (x, y) = [(a, b) | a <- [max 0 (x -1) .. min maxX (x + 1)], b <- [max 0 (y -1) .. min maxY (y + 1)], isNeighboor (a, b) (x, y)]

selectNextMoves :: Set Coordinate -> [(Coordinate, Coordinate)] -> Set Coordinate -> [(Coordinate, Coordinate)]
selectNextMoves targets availableTransfers visited =
  let selectedTransfers = filter ((`Set.member` targets) . fst) availableTransfers
  in if null selectedTransfers
     then selectNextMoves (((`Set.difference` visited) . Set.unions . Set.map (Set.fromList . neighboors)) targets) availableTransfers (Set.union visited targets)
     else selectedTransfers

performTransfer :: Grid -> (Coordinate, Coordinate) -> Grid
performTransfer grid (x, y) =
  let (n1, n2) = nodeTransfer (lookup' x grid, lookup' y grid)
      insertMap = fromList [(x, n1), (y, n2)]
   in Map.union insertMap grid

nextStatusAll :: Status -> [((Coordinate, (Coordinate, Coordinate)), Status)]
nextStatusAll (t, grid) =
  let transfers = filter ((t, second (+ 1) t) /=) $ possibleTransfers grid
  in fmap
      ( \(c1, c2) ->
          let nextGrid = performTransfer grid (c1, c2)
           in if c1 == t then ((t, (c1, c2)), (c2, nextGrid)) else ((t, (c1, c2)), (t, nextGrid))
      )
      transfers

nextStatus :: Status -> [((Coordinate, (Coordinate, Coordinate)), Status)]
nextStatus (t, grid) =
  let nextTargetMove = first (\x -> x - 1) t
      transfers = filter ((t, second (+ 1) t) /=) $ possibleTransfers grid
      nextMoves = if (t, nextTargetMove) `elem` transfers then [(t, nextTargetMove)] else selectNextMoves (Set.singleton nextTargetMove) transfers (Set.singleton t)
  in fmap
      ( \(c1, c2) ->
          let nextGrid = performTransfer grid (c1, c2)
           in if c1 == t then ((t, (c1, c2)), (c2, nextGrid)) else ((t, (c1, c2)), (t, nextGrid))
      )
      nextMoves

solution2 :: [Status] -> Status -> Map Coordinate (Set (Coordinate, Coordinate)) -> Int
solution2 chain status history
  | ((== (0, 0)) . fst) status = length chain
  | otherwise =
    let nextSs = nextStatus status
        maybeNextS = (find (\((t, c), _) -> (not . Set.member c) ((fromJust . Map.lookup t) history)) . sortOn (snd . snd)) nextSs
        nextSs' = nextStatusAll status
        maybeNextS' = (find (\((t, c), _) -> (not . Set.member c) ((fromJust . Map.lookup t) history)) . sortOn (snd . snd)) nextSs'
    in if isNothing maybeNextS
       then solution2 (takeWhile (status /=) chain ++ [status]) ((snd . fromJust) maybeNextS') (foldl (\h ((t, c), _) -> adjust (Set.insert c) t h) history maybeNextS')
      else solution2 (takeWhile (status /=) chain ++ [status]) ((snd . fromJust) maybeNextS) (foldl (\h ((t, c), _) -> adjust (Set.insert c) t h) history maybeNextS)

twentysecondDecemberSolution2 :: IO Int
twentysecondDecemberSolution2 = input <&> \x -> solution2 [] (target, x) startingHistory
  where
    startingHistory = fromList [((x, 0), Set.empty) | x <- [0 .. maxX]]

inputTest :: Grid
inputTest =
  (fromList . fmap parseNode . lines)
    "/dev/grid/node-x0-y0   10T    8T     2T   80%\n\
    \/dev/grid/node-x0-y1   11T    6T     5T   54%\n\
    \/dev/grid/node-x0-y2   32T   28T     4T   87%\n\
    \/dev/grid/node-x1-y0    9T    7T     2T   77%\n\
    \/dev/grid/node-x1-y1    8T    0T     8T    0%\n\
    \/dev/grid/node-x1-y2   11T    7T     4T   63%\n\
    \/dev/grid/node-x2-y0   10T    6T     4T   60%\n\
    \/dev/grid/node-x2-y1    9T    8T     1T   88%\n\
    \/dev/grid/node-x2-y2    9T    6T     3T   66%"

test :: Bool
test = solution2 [] ((2, 0), inputTest) startingHistory == 7
  where
    startingHistory = fromList [((x, 0), Set.empty) | x <- [0 .. 2]]
