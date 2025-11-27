module TwentyTwentyOne.December01 where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2021/1December.txt"

solution1 :: [Int] -> Int
solution1 mesurements =
    length . filter id . fmap (uncurry (<)) $ mesurements `zip` tail mesurements

testInput :: [Int]
testInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

december01Solution1 :: IO Int
december01Solution1 = solution1 <$> input

solution2 :: [Int] -> Int
solution2 mesurements =
    solution1 $ fmap (\(a, b, c) -> a + b + c) (zipTail3 mesurements)

zipTail3 :: [Int] -> [(Int, Int, Int)]
zipTail3 (z : y : x : xs) = (z, y, x) : zipTail3 (y : x : xs)
zipTail3 (y : x : xs) = []
zipTail3 (x : xs) = []
zipTail3 [] = []

december01Solution2 :: IO Int
december01Solution2 = solution2 <$> input
