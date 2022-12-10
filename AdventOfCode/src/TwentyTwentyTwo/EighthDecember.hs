module TwentyTwentyTwo.EighthDecember where

import Data.Bifunctor (bimap)
import Data.Map (Map, empty, filterWithKey, findMax, findMin, insert, mapWithKey, size, (!))
import Debug.Trace

type Position = (Int, Int)

newtype Tree = Tree {height :: Int} deriving (Show)

input :: IO (Map Position Tree)
input = parseInput <$> readFile "input/2022/8December.txt"

parseInput :: String -> Map Position Tree
parseInput = buildMap empty 0 . lines
  where
    buildMap :: Map Position Tree -> Int -> [String] -> Map Position Tree
    buildMap m _ [] = m
    buildMap m y (t : ts) =
        ( (\m' -> buildMap m' (y + 1) ts)
            . fst
            . foldl (\(m', x) h -> (insert (x, y) (Tree{height = h}) m', x + 1)) (m, 0)
            . fmap (\x -> read [x] :: Int)
        )
            t

isVisible :: Map Position Tree -> Position -> Tree -> Bool
isVisible m p (Tree{height = h}) =
    let (top, bottom) = extractColumn p m
        (left, right) = extractRow p m
        directionScenicScore = all (\(Tree{height = h'}) -> h' < h)
     in directionScenicScore top || directionScenicScore bottom || directionScenicScore left || directionScenicScore right

extractColumn :: Position -> Map Position Tree -> ([Tree], [Tree])
extractColumn (x, y) m =
    let ((_, minY), _) = findMin m
        ((_, maxY), _) = findMax m
        coords = (reverse [minY .. (y - 1)], [(y + 1) .. maxY])
     in bimap (fmap (\y' -> m ! (x, y'))) (fmap (\y' -> m ! (x, y'))) coords

extractRow :: Position -> Map Position Tree -> ([Tree], [Tree])
extractRow (x, y) m =
    let ((minX, _), _) = findMin m
        ((maxX, _), _) = findMax m
        coords = (reverse [minX .. (x - 1)], [(x + 1) .. maxX])
     in bimap (fmap (\x' -> m ! (x', y))) (fmap (\x' -> m ! (x', y))) coords

solution1 :: Map Position Tree -> Int
solution1 m = (size . filterWithKey (isVisible m)) m

testInput :: String
testInput =
    "30373\n\
    \25512\n\
    \65332\n\
    \33549\n\
    \35390"

eighthDecemberSolution1 :: IO Int
eighthDecemberSolution1 = solution1 <$> input

calculateScenicScore :: Map Position Tree -> Position -> Tree -> Int
calculateScenicScore m p (Tree{height = h}) =
    let (top, bottom) = extractColumn p m
        (left, right) = extractRow p m
        directionScenicScore xs = ((\l -> if l == length xs then l else l + 1) . length . takeWhile ((< h) . height)) xs
     in directionScenicScore top
            * directionScenicScore bottom
            * directionScenicScore left
            * directionScenicScore right

solution2 :: Map Position Tree -> Int
solution2 m = (maximum . mapWithKey (calculateScenicScore m)) m

eighthDecemberSolution2 :: IO Int
eighthDecemberSolution2 = solution2 <$> input
