module TwentyFifteen.December24 where

import Data.List

input :: IO [Int]
input =
    fmap (\x -> read x :: Int) . lines <$> readFile "input/2015/24December.txt"

weightPerGroup :: [Int] -> Int
weightPerGroup = (`div` 3) . sum

groupByWeight' :: [Int] -> [Int] -> Int -> [[Int]]
groupByWeight' acc _ 0 = [acc]
groupByWeight' _ [] _ = []
groupByWeight' acc (l : ls) target
    | sum (acc ++ [l]) == target = (acc ++ [l]) : groupByWeight' acc ls target
    | sum (acc ++ [l]) < target =
        groupByWeight' (acc ++ [l]) ls target ++ groupByWeight' acc ls target
    | sum (acc ++ [l]) > target = groupByWeight' acc ls target

selectSolutionByQuantumEntanglement' :: [[Int]] -> [Int]
selectSolutionByQuantumEntanglement' =
    minimumBy (\x x' -> compare (product x) (product x'))
        . head
        . groupBy (\x x' -> length x == length x')
        . sortOn length

inputTest :: [Int]
inputTest = [1 .. 5] ++ [7 .. 11]

solution1Test :: Bool
solution1Test = solution (weightPerGroup inputTest) inputTest == 99

solution :: Int -> [Int] -> Int
solution targetWeight xs =
    (product . selectSolutionByQuantumEntanglement') $
        groupByWeight' [m] (delete m xs) targetWeight
  where
    m = maximum xs

december24Solution1 :: IO Int
december24Solution1 =
    (\xs -> solution (weightPerGroup xs) xs) <$> input

weightPerGroup2 :: [Int] -> Int
weightPerGroup2 = (`div` 4) . sum

solution2Test :: Bool
solution2Test = solution (weightPerGroup2 inputTest) inputTest == 44

december24Solution2 :: IO Int
december24Solution2 =
    (\xs -> solution (weightPerGroup2 xs) xs) <$> input

-- EXTRA FUNCTIONS NOT USED :) ----------------
groupByThree :: [Int] -> Maybe [([Int], [Int], [Int])]
groupByThree xs = do
    let targetWeight = weightPerGroup xs
    (firstGroup, rest) <- groupPacks xs [] targetWeight
    (secondGroup, rest') <- groupPacks rest [] targetWeight
    (thirdGroup, rest'') <- groupPacks rest' [] targetWeight
    if null rest''
        then
            return
                [ (firstGroup, secondGroup, thirdGroup) -- not generating all, only the first group is relevant
                , (secondGroup, firstGroup, thirdGroup)
                , (thirdGroup, firstGroup, secondGroup)
                ]
        else Nothing

groupByThree' :: [[Int]] -> [([Int], [Int], [Int])]
groupByThree' xss =
    [(a, fst b, snd b) | a <- xss, b <- uniqueTuples (delete a xss)]
  where
    uniqueTuples :: [[Int]] -> [([Int], [Int])]
    uniqueTuples [] = []
    uniqueTuples [_] = []
    uniqueTuples (ys : yss) = [(ys, y) | y <- yss] ++ uniqueTuples yss

groupPacks :: [Int] -> [Int] -> Int -> Maybe ([Int], [Int])
groupPacks [] acc _ = Just (acc, [])
groupPacks a@(x : xs) acc target
    | sum acc == target = Just (acc, a)
    | sum acc > target = Nothing
    | otherwise = groupPacks xs (acc ++ [x]) target

groupByWeight :: [Int] -> [[Int]]
groupByWeight xs =
    let allByWeight =
            xs >>= \x -> groupByWeight' [x] (delete x xs) (weightPerGroup xs)
     in (nub . fmap sort) allByWeight

selectSolutionByQuantumEntanglement ::
    [([Int], [Int], [Int])] -> ([Int], [Int], [Int])
selectSolutionByQuantumEntanglement =
    minimumBy (\(x, _, _) (x', _, _) -> compare (product x) (product x'))
        . head
        . groupBy (\(x, _, _) (x', _, _) -> length x == length x')
        . sortOn (\(x, _, _) -> length x)
