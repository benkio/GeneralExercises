module TwentyTwentyFour.December22 where

import Control.Arrow
import Control.Parallel
import Control.Parallel.Strategies
import Data.Containers.ListUtils (nubOrd)
import Data.Functor ((<&>))
import Data.Map (Map)
import qualified Data.Map as M (empty, fromList, insert, keys, (!?))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set hiding (foldl, take)
import qualified Data.Set as S (foldl, take)
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

secretDifferencesAndPrices :: Int -> ([Int], [Int])
secretDifferencesAndPrices s =
    ( (\xs -> zipWith (\x y -> firstDigit y - firstDigit x) xs (tail xs)) prices
    , prices
    )
  where
    prices = (take 2001 . nextSecrets) s

firstDigit :: Int -> Int
firstDigit = read . (: []) . last . show

monkeySequnces :: [Int] -> [[Int]]
monkeySequnces = slidingWindow 4

findBananaPrice :: [Int] -> [[Int]] -> [Int] -> Int
findBananaPrice sequence sellerDiffSequences sellerPrices =
    maybe
        0
        firstDigit
        (maybePriceIndex 0 sellerDiffSequences >>= (sellerPrices !?))
  where
    maybePriceIndex _ [] = Nothing
    maybePriceIndex firstIndex (x : xs)
        | x == sequence = Just $ firstIndex + 4
        | otherwise = maybePriceIndex (firstIndex + 1) xs

seqBananaPrice :: [Int] -> [([[Int]], [Int])] -> Int
seqBananaPrice sequence =
    foldl go 0
  where
    go acc = (acc +) . uncurry (findBananaPrice sequence)

mapBananaPrice :: ([[Int]], [Int]) -> Map [Int] Int
mapBananaPrice (sequences, prices) =
    foldl (\m s -> M.insert s (findBananaPrice s sequences prices) m) M.empty sequences

test = solution2 testInput'

solution2 :: [Int] -> Int
solution2 i =
    S.foldl go 0 allSequences
  where
    sellersMap = trace "sellers Computed" $ fmap (mapBananaPrice . first monkeySequnces . secretDifferencesAndPrices) i
    allSequences = (\x -> trace ("allSequences Computed: " ++ show (size x)) x) . fromList . concatMap M.keys $ sellersMap
    go result sequence =
        ( \x ->
            trace
                (" sequence: " ++ show sequence ++ " result: " ++ show x)
                x
        )
            (max result (sum (mapMaybe (M.!? sequence) sellersMap)))

december22Solution2 :: IO Int
december22Solution2 = solution2 <$> input
