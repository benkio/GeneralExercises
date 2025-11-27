module TwentyFifteen.December10 where

import Data.List (group)

input :: IO Int
input = read <$> readFile "input/2015/10December.txt"

inputTest :: Integer
inputTest = 1

generateLookAndSay :: Integer -> [Integer]
generateLookAndSay = iterate generateNextLookAndSay

generateNextLookAndSay :: Integer -> Integer
generateNextLookAndSay =
    read . concatMap (\l -> show (length l) ++ [head l]) . group . show

solution1Test :: Bool
solution1Test = (== 312211) $ solution 5 1

solution :: Int -> Int -> Integer
solution iterations = (!! iterations) . generateLookAndSay . toInteger

december10Solution1 :: IO Int
december10Solution1 = length . show . solution 40 <$> input

december10Solution2 :: IO Int
december10Solution2 = length . show . solution 50 <$> input
