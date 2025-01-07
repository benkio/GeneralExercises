module TwentyTwentyFour.December22 where

import Control.Arrow
import Control.Parallel
import Control.Parallel.Strategies
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set, empty, fromList, toList)
import qualified Data.Set as S (union)
import Debug.Trace
import Lib.Bit (bitWiseXor)
import Lib.List (slidingWindow, (!?))

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

testInput' :: [Int]
testInput' =
    parseInput
        "1\n\
        \2\n\
        \3\n\
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

solution1 :: [Int] -> Int
solution1 = sum . parMap rdeepseq endOfTheDaySecret

december22Solution1 :: IO Int
december22Solution1 = solution1 <$> input

secretDifferences :: Int -> [Int]
secretDifferences = (\xs -> zipWith (\x y -> firstDigit y - firstDigit x) xs (tail xs)) . take 2001 . nextSecrets

firstDigit :: Int -> Int
firstDigit = read . (: []) . last . show

monkeySequnces :: [Int] -> [[Int]]
monkeySequnces = slidingWindow 4

allMonkeySequences :: [[Int]] -> Set [Int]
allMonkeySequences = foldl (\acc x -> acc `S.union` ((fromList . monkeySequnces) x)) empty

findBananaPrice :: [Int] -> [[Int]] -> [Int] -> Int
findBananaPrice sequence sellerDiffSequences sellerPrices =
    fromMaybe 0 $ (maybePriceIndex 0 sellerDiffSequences) >>= (sellerPrices !?) <&> firstDigit
  where
    maybePriceIndex _ [] = Nothing
    maybePriceIndex firstIndex (x : xs)
        | x == sequence = Just $ firstIndex + 4
        | otherwise = maybePriceIndex (firstIndex + 1) xs

seqBananaPrice :: [Int] -> [([[Int]], [Int])] -> Int
seqBananaPrice sequence sellers =
    foldl go 0 sellers
  where
    go acc = (acc +) . uncurry (findBananaPrice sequence)

test =
    seqBananaPrice [-2, 1, -1, 3] (sellerDiffSequences `zip` sellerPrices)
  where
    sellerDiffSequences = fmap (monkeySequnces . secretDifferences) testInput'
    sellerPrices = fmap (take 2001 . nextSecrets) testInput'

solution2 :: [Int] -> Int
solution2 = undefined

december22Solution2 :: IO Int
december22Solution2 = solution2 <$> input
