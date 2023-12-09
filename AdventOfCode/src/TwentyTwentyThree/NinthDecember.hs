module TwentyTwentyThree.NinthDecember where

input :: IO [[Int]]
input = parseInput <$> readFile "input/2023/9December.txt"

parseInput :: String -> [[Int]]
parseInput = fmap (fmap (\x -> read x :: Int) . words) . lines

testInput :: [[Int]]
testInput =
    parseInput
        "0 3 6 9 12 15\n\
        \1 3 6 10 15 21\n\
        \10 13 16 21 30 45"

genDiffs :: [Int] -> [Int]
genDiffs xs = zipWith (flip (-)) xs (tail xs)

genDiffPyramid :: [Int] -> [[Int]]
genDiffPyramid xs
    | all (== 0) xs = []
    | otherwise = diffs : genDiffPyramid diffs
  where
    diffs = genDiffs xs

foldDiffPyramid :: ([Int] -> Int -> Int) -> [[Int]] -> Int
foldDiffPyramid f = foldr f 0 . init

solution :: ([Int] -> Int -> Int) -> [[Int]] -> Int
solution f = sum . fmap (\xs -> foldDiffPyramid f (xs : genDiffPyramid xs))

-- too high 1593394417
ninthDecemberSolution1 :: IO Int
ninthDecemberSolution1 = solution (\xs y -> y + last xs) <$> input

ninthDecemberSolution2 :: IO Int
ninthDecemberSolution2 = solution (\xs y -> head xs - y) <$> input
