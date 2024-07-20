{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ParallelListComp #-}

module TwentyTwentyThree.TwentyFirstDecember where

import Text.Printf (printf)

import Data.List
import Data.Map (Map)
import qualified Data.Map as M (filter, findMax, fromList, lookup, mapKeys, size, toList, union)
import Data.Set (Set)
import qualified Data.Set as S (empty, foldr, fromList, insert, map, size, toList)
import Debug.Trace

data FieldBlock = GardenPlot | Rock | StartingPosition deriving (Show, Eq)
data Step = N | S | E | W deriving (Show, Enum)
type Field = Map (Int, Int) FieldBlock

setConcatMap :: (Ord b) => (a -> [b]) -> Set a -> Set b
setConcatMap f = S.fromList . concatMap f . S.toList

input :: IO Field
input = parseInput <$> readFile "input/2023/21December.txt"

isAvailableStep :: Field -> (Int, Int) -> Maybe Bool
isAvailableStep f x = (\x -> x == StartingPosition || x == GardenPlot) <$> M.lookup x f

takeStep :: (Int, Int) -> Step -> (Int, Int)
takeStep (x, y) N = (x, y - 1)
takeStep (x, y) S = (x, y + 1)
takeStep (x, y) E = (x - 1, y)
takeStep (x, y) W = (x + 1, y)

invertStep :: (Int, Int) -> Step -> (Int, Int)
invertStep (x, y) N = (x, y + 1)
invertStep (x, y) S = (x, y - 1)
invertStep (x, y) E = (x + 1, y)
invertStep (x, y) W = (x - 1, y)

takeOneStep :: Field -> Set (Int, Int) -> Set (Int, Int)
takeOneStep f =
    setConcatMap
        ( \c ->
            filter
                (\x -> isAvailableStep f x == Just True)
                (takeStep c <$> enumFrom N)
        )

walk :: Field -> Set (Int, Int) -> Int -> Set (Int, Int)
walk _ s 0 = s
walk f s c = walk f (takeOneStep f s) (c + 1)

solution1 f = S.size $ walk f startingPoint 64
  where
    startingPoint :: Set (Int, Int)
    startingPoint = S.fromList . map fst . M.toList $ M.filter (== StartingPosition) f

twentyfirstDecemberSolution1 :: IO Int
twentyfirstDecemberSolution1 = solution1 <$> input

expandField :: Field -> (Int, Int) -> Field -> Step -> Field
expandField f c f' s = expandField' f (amendCoord c s) f' s
  where
    (mx, my) = fst $ M.findMax f'
    amendCoord (x, y) N = (x - (x `mod` (mx + 1)), y)
    amendCoord (x, y) S = (x - (x `mod` (mx + 1)), y)
    amendCoord (x, y) E = (x, y - (y `mod` (my + 1)))
    amendCoord (x, y) W = (x, y - (y `mod` (my + 1)))

expandField' :: Field -> (Int, Int) -> Field -> Step -> Field
expandField' f (sx, sy) f' N = f `M.union` M.mapKeys (\(x, y) -> (sx + x, sy + y - (snd . fst . M.findMax) f' - 1)) f'
expandField' f (sx, sy) f' S = f `M.union` M.mapKeys (\(x, y) -> (sx + x, sy + y + 1)) f'
expandField' f (sx, sy) f' W = f `M.union` M.mapKeys (\(x, y) -> (sx + x + 1, sy + y)) f'
expandField' f (sx, sy) f' E = f `M.union` M.mapKeys (\(x, y) -> (sx + x - (fst . fst . M.findMax) f' - 1, sy + y)) f'

takeOneStepInfinite :: Field -> Set (Int, Int) -> Field -> (Set (Int, Int), Field)
takeOneStepInfinite f s fieldBlock =
    S.foldr step (S.empty, f) s
  where
    step :: (Int, Int) -> (Set (Int, Int), Field) -> (Set (Int, Int), Field)
    step c (s, f) = foldr (step' . (\ d -> (d, takeStep c d))) (s, f) (enumFrom N)
    step' :: (Step, (Int, Int)) -> (Set (Int, Int), Field) -> (Set (Int, Int), Field)
    step' (direction, c) (s, f)
        | isAvailableStep f c == Just True = (S.insert c s, f)
        | isNothing (M.lookup c f) = step' (direction, c) (s, expandField f (invertStep c direction) fieldBlock direction)
        | otherwise = (s, f)

findPolynomialValues :: Field -> Set (Int, Int) -> Field -> Int -> Int -> [Int] -> [Int]
findPolynomialValues f !s fieldBlock c c' xs
    | c == c' = xs
    | otherwise =
        let (s', f') = takeOneStepInfinite f s fieldBlock
         in if c == 65 || c `mod` 131 == 65
                then findPolynomialValues f' s' fieldBlock (c + 1) c' (xs ++ [S.size s])
                else findPolynomialValues f' s' fieldBlock (c + 1) c' xs

solution2 :: Field -> Int
solution2 f = (xTarget ^ 2) * a + xTarget * b + c
  where
    startingPoint :: Set (Int, Int)
    startingPoint = S.fromList . map fst . M.toList $ M.filter (== StartingPosition) f
    xTarget = (26501365 - 65) `div` 131
    [c, b, a] = iterativePolyFit $ zip [0 ..] $ findPolynomialValues f startingPoint f 0 (65 + 131 * 2 + 1) []

-- 620348631910321
twentyfirstDecemberSolution2 :: IO Int
twentyfirstDecemberSolution2 = solution2 <$> input

parseInput :: String -> Field
parseInput = M.fromList . concat . zipWith (\y ls -> zipWith (\x c -> ((x, y), fromChar c)) [0 ..] ls) [0 ..] . lines
  where
    fromChar :: Char -> FieldBlock
    fromChar '.' = GardenPlot
    fromChar '#' = Rock
    fromChar 'S' = StartingPosition

testInput :: Field
testInput =
    parseInput
        "...........\n\
        \.....###.#.\n\
        \.###.##..#.\n\
        \..#.#...#..\n\
        \....#.#....\n\
        \.##..S####.\n\
        \.##..#...#.\n\
        \.......##..\n\
        \.##.#.####.\n\
        \.##..##.##.\n\
        \..........."

{- | Evaluate a polynomial passing through the specified set of points.  The
 order of the interpolating polynomial will (at most) be one less than
 the number of points given.
-}
polyInterp :: [(Int, Int)] -> Int -> Int
polyInterp xys = head . last . neville xys

{- | Computes the tableau generated by Neville's algorithm.  Each successive
 row of the table is a list of interpolants one order higher than the previous,
 using a range of input points starting at the same position in the input
 list as the interpolant's position in the output list.
-}
neville :: [(Int, Int)] -> Int -> [[Int]]
neville xys x = table
  where
    (xs, ys) = unzip xys
    table =
        ys
            : [ [ ((x - x_j) * p1 + (x_i - x) * p0) `div` (x_i - x_j)
                | p0 : p1 : _ <- tails row
                | x_j <- xs
                | x_i <- x_is
                ]
              | row <- table
              | x_is <- tail (tails xs)
              , not (null x_is)
              ]

iterativePolyFit :: [(Int, Int)] -> [Int]
iterativePolyFit = loop
  where
    loop [] = []
    loop xys = c0 : loop (drop 1 xys')
      where
        c0 = polyInterp xys 0
        xys' =
            [ (x, (y - c0) `div` x)
            | (x, y) <- xys
            ]
