{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December11 where

import Data.Bifunctor (bimap, first, second)
import Data.List (groupBy, sortOn)
import Data.Map (Map, elems, empty, filterWithKey, fromList, keys, singleton, toList, unionWith, (!?))

-- improvement only use Int to keep tracks of zeroes and 2024
data Stones = S
    { r1d :: !Int
    , r2d :: Map String Int
    , r3d :: Map String Int
    }
    deriving (Show)

input :: IO Stones
input = classifyStones . words <$> readFile "input/2024/December11.txt"

testInput :: Stones
testInput =
    (classifyStones . words)
        "125 17\n"

classifyStones :: [String] -> Stones
classifyStones xs =
    S
        { r1d = r1Data
        , r2d = r2Data
        , r3d = r3Data
        }
  where
    (r1Data, r2Data, r3Data) = listToStoneClassify $ fmap (,1) xs

listToStoneClassify :: [(String, Int)] -> (Int, Map String Int, Map String Int)
listToStoneClassify xs = (r1Data, r2Data, r3Data)
  where
    r1Data = sum $ snd <$> filter (rule1Check . fst) xs
    toMap =
        fromList
            . fmap (\ys -> (fst (head ys), (sum . fmap snd) ys))
            . groupBy (\(k, _) (k', _) -> k == k')
            . sortOn fst
    r2Data = toMap . filter (rule2Check . fst) $ xs
    r3Data = toMap . filter (rule3Check . fst) $ xs

mapStoneClassify :: Map String Int -> (Int, Map String Int, Map String Int)
mapStoneClassify m = (zeros, r2map, r3map)
  where
    zeros = sum . elems . filterWithKey (\k _ -> rule1Check k) $ m
    r2map = filterWithKey (\k _ -> rule2Check k) m
    r3map = filterWithKey (\k _ -> rule3Check k) m

rule1Check :: String -> Bool
rule1Check = (== "0")
rule2 :: String -> (String, String)
rule2 s =
    fmap (\xs -> if null xs then "0" else xs)
        . bimap (dropWhile (== '0')) (dropWhile (== '0'))
        . splitAt (length s `div` 2)
        $ s
rule2Check :: String -> Bool
rule2Check s = even (length s) && not (rule1Check s)
rule3 :: String -> String
rule3 s = show $ 2024 * (read s :: Int)
rule3Check :: String -> Bool
rule3Check s = all (\f -> not (f s)) [rule1Check, rule2Check]

evolveStones :: Stones -> Stones
evolveStones (S{r1d = r1Data, r2d = r2Data, r3d = r3Data}) =
    S
        { r1d = r2DataEv1 + r3DataEv1
        , r2d = unionWith (+) r2DataEv2 r3DataEv2
        , r3d = unionWith (+) (unionWith (+) r1DataEv r2DataEv3) r3DataEv3
        }
  where
    -- r1Data goes in r3d as everyone becomes 1
    r1DataEv = if r1Data > 0 then singleton "1" r1Data else empty
    (r2DataEv1, r2DataEv2, r2DataEv3) =
        listToStoneClassify
            . concatMap (\(k, v) -> (\(s, s') -> [(s, v), (s', v)]) (rule2 k))
            $ toList r2Data
    (r3DataEv1, r3DataEv2, r3DataEv3) = listToStoneClassify $ first rule3 <$> toList r3Data

howManyStones :: Stones -> Int
howManyStones S{r1d = r1Data, r2d = r2Data, r3d = r3Data} =
    (r1Data +) . sum $ fmap sum [r2Data, r3Data]

solution :: Int -> Stones -> Int
solution n =
    howManyStones
        . (!! n)
        . iterate evolveStones

december11Solution1 :: IO Int
december11Solution1 = solution 25 <$> input

december11Solution2 :: IO Int
december11Solution2 = solution 75 <$> input
