module TwentyTwentyOne.FirstDecember where

input :: IO [Int]
input = fmap (\x -> (read x :: Int)) . lines <$> readFile "input/2021/1December.txt"

solution1 :: [Int] -> Int
solution1 mesurements =
  length . filter id . fmap (uncurry (<)) $ mesurements `zip` tail mesurements

testInput :: [Int]
testInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

firstDecemberSolution1 :: IO Int
firstDecemberSolution1 = solution1 <$> input

solution2 :: [Int] -> Int
solution2 mesurements =
  solution1 $ fmap (\(a, b, c) -> a + b + c) (zipTail3 mesurements)

zipTail3 :: [Int] -> [(Int, Int, Int)]
zipTail3 (z:y:x:xs) = (z,y,x) : zipTail3 (y:x:xs)
zipTail3 (y:x:xs) = []
zipTail3 (x:xs) = []
zipTail3 [] = []

firstDecemberSolution2 :: IO Int
firstDecemberSolution2 = solution2 <$> input
