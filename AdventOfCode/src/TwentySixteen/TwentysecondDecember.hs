{-# LANGUAGE TupleSections #-}

module TwentySixteen.TwentysecondDecember where

import Data.List
import Data.Bifunctor
import Data.Map (Map, fromList, toList, union)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set, singleton)
import qualified Data.Set as Set (difference, empty, fromList, map, member, union, unions)
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

selectNextMoves :: Set Coordinate -> [(Coordinate, Coordinate)] -> Set Coordinate -> IO [(Coordinate, Coordinate)]
selectNextMoves targets availableTransfers visited = do
  let selectedTransfers = filter ((`Set.member` targets) . fst) availableTransfers
--  print $ "target " ++ show targets ++ " - visited " ++ show visited
  if null selectedTransfers
    then selectNextMoves (((`Set.difference` visited) . Set.unions . Set.map (Set.fromList . neighboors)) targets) availableTransfers (Set.union visited targets)
    else 
         (return
      . concatMap (\l ->
                     foldl (\_ x -> [x]) l (find (\((_, y), (_, y')) -> y < y') l)
                     -- foldl (\acc (c2, b) ->
                         --     case compare ((snd . fst . last) acc) (snd c2) of
                         --       LT -> acc
                         --       GT -> [(c2, b)]
                         --       EQ -> acc ++ [(c2, b)]
                         --   ) [head l] (tail l)
                  )
      . groupBy (\(_, x)  (_, y) -> x == y)
      . sortOn snd )
    selectedTransfers

performTransfer :: Grid -> (Coordinate, Coordinate) -> Grid
performTransfer grid (x, y) =
  let (n1, n2) = nodeTransfer (lookup' x grid, lookup' y grid)
      insertMap = fromList [(x, n1), (y, n2)]
   in Map.union insertMap grid

nextStatus :: Status -> IO [Status]
nextStatus (t, grid) = do
  let nextTargetMove = first (\x -> x - 1) t
      transfers = possibleTransfers grid
  nextMoves <- if (t, nextTargetMove) `elem` transfers then return [(t, nextTargetMove)] else selectNextMoves (singleton nextTargetMove) transfers (singleton t)
  print ("nm " ++ show nextMoves)
  return $
    fmap
      ( \(c1, c2) ->
          let nextGrid = performTransfer grid (c1, c2)
           in if c1 == t then (c2, nextGrid) else (t, nextGrid)
      )
      nextMoves

solution2 :: Int -> [Status] -> Set Status -> IO Int
solution2 _ [] _ = error "no next status generated"
solution2 steps statuses history
  | any ((== (0, 0)) . fst) statuses = return steps
  | otherwise = do
      nextStatuses'' <- traverse nextStatus statuses
      let nextStatuses' = concat nextStatuses''
          nextStatuses = filter (`notElem` history) nextStatuses'
      (print . length) nextStatuses'
      (print . length) nextStatuses
      (print . fst . last) nextStatuses
    --    _ <- getLine
      solution2 (steps + 1) nextStatuses (Set.union (Set.fromList nextStatuses) history)

twentysecondDecemberSolution2 :: IO Int
twentysecondDecemberSolution2 = input >>= \x -> solution2 0 [(target, x)] Set.empty

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

test :: IO Bool
test = (== 7) <$> solution2 0 [((2, 0), inputTest)] Set.empty
