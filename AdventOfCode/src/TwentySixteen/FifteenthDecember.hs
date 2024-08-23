module TwentySixteen.FifteenthDecember where

input :: IO [[Int]]
input = fmap parseDisk . lines <$> readFile "input/2016/15December.txt"

parseDisk :: String -> [Int]
parseDisk s =
    let totPosition = (read (words s !! 3) :: Int) - 1
        startingPosition = ((\x -> read x :: Int) . init . last) (words s)
     in [startingPosition .. totPosition] ++ cycle [0 .. totPosition]

solution1 :: Int -> [[Int]] -> Int
solution1 count xs
    | endCondition xs = count
    | otherwise = solution1 (count + 1) (fmap tail xs)

endCondition :: [[Int]] -> Bool
endCondition = all ((0 ==) . (\(i, xs) -> xs !! i)) . zip [1 ..]

testInput :: [[Int]]
testInput =
    (fmap parseDisk . lines)
        "Disc #1 has 5 positions; at time=0, it is at position 4.\n\
        \Disc #2 has 2 positions; at time=0, it is at position 1."

testSolution1 :: Bool
testSolution1 = solution1 0 testInput == 5

fifteenthDecemberSolution1 :: IO Int
fifteenthDecemberSolution1 = solution1 0 <$> input

newDisk :: [Int]
newDisk = parseDisk "Disc #7 has 11 positions; at time=0, it is at position 0."

fifteenthDecemberSolution2 :: IO Int
fifteenthDecemberSolution2 = solution1 0 . (\l -> l ++ [newDisk]) <$> input
