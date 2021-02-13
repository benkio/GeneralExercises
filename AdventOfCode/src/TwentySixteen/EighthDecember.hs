module TwentySixteen.EighthDecember where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type Coordinate = (Int, Int)

type Grid = Map Coordinate Bool

data Instruction
  = Rect Int Int
  | RotateRow Int Int
  | RotateColumn Int Int
  deriving (Show)

input :: IO [Instruction]
input = fmap parseInstruction . lines <$> readFile "input/2016/8December.txt"

parseInstruction :: String -> Instruction
parseInstruction s
  | "rect " `isPrefixOf` s =
    ((\x ->
        Rect
          (read (takeWhile ('x' /=) x) :: Int)
          (read ((tail . dropWhile ('x' /=)) x) :: Int)) .
     fromJust . stripPrefix "rect ")
      s
  | "rotate row y=" `isPrefixOf` s =
    ((\x ->
        RotateRow
          (read (takeWhile (' ' /=) x) :: Int)
          (read ((drop 2 . dropWhile ('y' /=)) x) :: Int)) .
     fromJust . stripPrefix "rotate row y=")
      s
  | "rotate column x=" `isPrefixOf` s =
    ((\x ->
        RotateColumn
          (read (takeWhile (' ' /=) x) :: Int)
          (read ((drop 2 . dropWhile ('y' /=)) x) :: Int)) .
     fromJust . stripPrefix "rotate column x=")
      s
  | otherwise = error $ "Unable to parse: " ++ s

createGrid :: Int -> Int -> Grid
createGrid width height =
  Map.fromList [((x, y), False) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

initialGrid :: Grid
initialGrid = createGrid 50 6

initialGridTest :: Grid
initialGridTest = createGrid 7 3

showGrid :: Grid -> String
showGrid m =
  (unlines .
   fmap
     (foldl
        (\acc' (_, c) ->
           acc' ++
           [ if c
               then '#'
               else '.'
           ])
        "") .
   groupBy (\((_, y), _) ((_, y'), _) -> y == y') .
   sortOn (\((x, y), _) -> x + y * ((+ 1) . fst . fst . Map.findMax) m) .
   Map.toList)
    m

applyInstruction :: Grid -> Instruction -> Grid
applyInstruction m (Rect x y) =
  Map.union (Map.map (const True) (createGrid x y)) m
applyInstruction m i@(RotateRow _ _) = Map.union (rowShift m i) m
applyInstruction m i@(RotateColumn _ _) = Map.union (columnShift m i) m

rowShift :: Grid -> Instruction -> Grid
rowShift m (RotateRow y p) =
  (Map.mapKeys
     (\(x', y') -> ((x' + p) `mod` ((+ 1) . fst . fst . Map.findMax) m, y')) .
   Map.filterWithKey (\(_, y') _ -> y' == y))
    m
rowShift m _ = m

columnShift :: Grid -> Instruction -> Grid
columnShift m (RotateColumn x p) =
  (Map.mapKeys
     (\(x', y') -> (x', (y' + p) `mod` ((+ 1) . snd . fst . Map.findMax) m)) .
   Map.filterWithKey (\(x', _) _ -> x' == x))
    m
columnShift m _ = m

solution :: Grid -> [Instruction] -> Grid
solution = foldl applyInstruction

countLitPixels :: Grid -> Int
countLitPixels = Map.size . Map.filter id

inputTest :: [Instruction]
inputTest =
  (fmap parseInstruction . lines)
    "rect 3x2\n\
\rotate column x=1 by 1\n\
\rotate row y=0 by 4\n\
\rotate column x=1 by 1"

solutionTest :: Bool
solutionTest = countLitPixels (solution initialGridTest inputTest) == 6

eighthDecemberSolution1 :: IO Int
eighthDecemberSolution1 = countLitPixels . solution initialGrid <$> input

eighthDecemberSolution2 :: IO String
eighthDecemberSolution2 = showGrid . solution initialGrid <$> input
