module TwentySixteen.TwentysecondDecember where

import Data.List (any)
import Data.Map (Map, fromList, toList, union)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set, singleton)
import qualified Data.Set as Set (union, empty, fromList)
import Text.Printf

type Coordinate = (Int, Int)

type Status = (Coordinate, Grid)

type Grid = Map Coordinate Node

data Node = Node
  { size :: Int,
    used :: Int,
    avail :: Int,
    usedPercent :: Int
  }
  deriving (Eq, Ord)

instance Show Node where
  show Node {size = s, used = u, avail = a, usedPercent = p} =
    printf "Node: s %d - u %d - a %d - p %d%%" s u a p

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
            avail = ((\x -> read x :: Int) . init) (l !! 3),
            usedPercent = ((\x -> read x :: Int) . init) (l !! 4)
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

--
-- ✓ 1 - Node list to Map by Coordinate
-- ✓ 2 - Node to Node transfer
-- ✓ 2b - possible transfers by neighboors
-- 3 - Grid -> Node -> Set Grid generate the grids if I can move the node
-- 4 - Solution: start from the target, generate the grids, if empty
-- then generate the grids for the neighboors (recursive until you got
-- something). Re apply solution with the target (if not moved) to the
-- new grids. When target = (0,0) stop
--
-- History?? later maybe
--

target :: Coordinate
target = (34, 0)

nodeTransfer :: (Node, Node) -> (Node, Node)
nodeTransfer (n1, n2) =
  ( n1 {used = 0, avail = size n1, usedPercent = 0},
    n2 {used = used n2 + used n1, avail = avail n2 - used n1, usedPercent = (used n2 + used n1) `div` (size n2 `div` 100)}
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

moveNode :: Coordinate -> (Coordinate -> Bool) -> Grid -> [Grid]
moveNode c targetFilter g =
  ( fmap
      ( \(x, y) ->
          let (n1, n2) = nodeTransfer (lookup' x g, lookup' y g)
              insertMap = fromList [(x, n1), (y, n2)]
           in union insertMap g
      )
      . filter (\(c', c'') -> c' == c && targetFilter c'')
      . possibleTransfers
  )
    g

nextStatus :: Status -> [Status]
nextStatus s = undefined

solution2 :: Int -> [Status] -> Set Status -> Int
solution2 steps statuses history
  | any ((== (0, 0)) . fst) statuses = steps
  | otherwise =
    let nextStatuses = (filter (`notElem` history) . concatMap nextStatus) statuses
     in solution2 (steps + 1) statuses (Set.union (Set.fromList nextStatuses) history)

twentysecondDecemberSolution2 :: IO Int
twentysecondDecemberSolution2 = undefined

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
test = solution2 0 [((2, 0), inputTest)] Set.empty  == 7
