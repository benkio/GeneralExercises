module TwentySixteen.ThirdDecember where

import Data.List

data Shape
    = Shape Int Int Int
    deriving (Show)

input :: IO [Shape]
input =
    fmap (parseShape . words) . lines <$> readFile "input/2016/3December.txt"

parseShape :: [String] -> Shape
parseShape ws =
    Shape (read (head ws) :: Int) (read (ws !! 1) :: Int) (read (ws !! 2) :: Int)

isTriangle :: Shape -> Bool
isTriangle (Shape x y z) = (x + y) > z && (x + z) > y && (z + y) > x

solution1 :: [Shape] -> [Shape]
solution1 = filter isTriangle

thirdDecemberSolution1 :: IO Int
thirdDecemberSolution1 = length . solution1 <$> input

inputCol :: IO [Shape]
inputCol =
    fmap parseShape . groupTriangles . transpose . fmap words . lines
        <$> readFile "input/2016/3December.txt"

groupTriangles :: (Eq a) => [[a]] -> [[a]]
groupTriangles [] = []
groupTriangles (x : xs) =
    filter ((== 3) . length) $ columnCombinations x ++ groupTriangles xs

columnCombinations :: (Eq a) => [a] -> [[a]]
columnCombinations [] = []
columnCombinations xs = take 3 xs : columnCombinations (drop 3 xs)

thirdDecemberSolution2 :: IO Int
thirdDecemberSolution2 = length . solution1 <$> inputCol
