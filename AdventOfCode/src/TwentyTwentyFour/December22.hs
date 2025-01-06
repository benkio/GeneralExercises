module TwentyTwentyFour.December22 where

import Control.Arrow
import Control.Parallel
import Control.Parallel.Strategies
import Lib.Bit (bitWiseXor)

input :: IO [Int]
input = parseInput <$> readFile "input/2024/December22.txt"

parseInput :: String -> [Int]
parseInput = fmap read . lines

testInput :: [Int]
testInput =
    parseInput
        "1\n\
        \10\n\
        \100\n\
        \2024\n"

mix :: (->) (Int, Int) Int
mix = arr $ uncurry bitWiseXor

prune :: (->) Int Int
prune = arr (`mod` 16777216)

nextSecretF :: (->) Int Int
nextSecretF =
    mixAndPrune (amul 64) >>> mixAndPrune (adiv 32) >>> mixAndPrune (amul 2048)
  where
    amul, adiv :: Int -> (->) Int Int
    amul m = arr (* m)
    adiv d = arr (`div` d)
    mixAndPrune :: (->) Int Int -> (->) Int Int
    mixAndPrune op =
        op &&& arr id >>> mix >>> prune

nextSecrets :: Int -> [Int]
nextSecrets = iterate nextSecretF

endOfTheDaySecret :: Int -> Int
endOfTheDaySecret = (!! 2000) . nextSecrets

secretDifferences :: Int -> [Int]
secretDifferences = (\xs -> zipWith (\x y -> firstDigit y - firstDigit x) xs (tail xs))  . take 2001 . nextSecrets
  where
    firstDigit :: Int -> Int
    firstDigit = read . (:[]) . last . show

solution1 :: [Int] -> Int
solution1 = sum . parMap rdeepseq endOfTheDaySecret

december22Solution1 :: IO Int
december22Solution1 = solution1 <$> input

solution2 :: [Int] -> Int
solution2 = undefined

december22Solution2 :: IO Int
december22Solution2 = solution2 <$> input
