module TwentyTwentyThree.FifthDecember where

import Data.List (groupBy)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (fromMaybe, maybeToList)
import Text.Printf (printf)

data FieldMap = FM {drs :: Int, srs :: Int, rl :: Int} deriving (Show)

data Field = F
    { seeds :: [Int]
    , seedToSoil :: [FieldMap]
    , soilToFertilizer :: [FieldMap]
    , fertilizerToWater :: [FieldMap]
    , waterToLight :: [FieldMap]
    , lightToTemperature :: [FieldMap]
    , temperatureToHumidity :: [FieldMap]
    , humitidyToLocation :: [FieldMap]
    }
    deriving (Show)

input :: IO Field
input = parseInput <$> readFile "input/2023/5December.txt"

sourceToDestination :: FieldMap -> Int -> Maybe Int
sourceToDestination (FM{drs = drs', srs = srs', rl = rl'}) s
    | s >= srs' && s < (srs' + rl') = Just $ drs' + (s - srs')
    | otherwise = Nothing

mapToDestination :: [FieldMap] -> Int -> Int
mapToDestination [] s = s
mapToDestination (fm : fms) s = fromMaybe (mapToDestination fms s) dst
  where
    dst = sourceToDestination fm s

fieldToDestination :: [[FieldMap]] -> Int -> Int
fieldToDestination fs s = foldl (flip mapToDestination) s fs

solution1 :: Field -> Int
solution1 f = minimum $ fmap (fieldToDestination fs) (seeds f)
  where
    fs = fieldMaps f

fifthDecemberSolution1 :: IO Int
fifthDecemberSolution1 = solution1 <$> input

type Range = (Int, Int)

sourceToDestination' :: FieldMap -> Range -> (Maybe Range, [Range])
sourceToDestination' (FM{drs = drs', srs = s', rl = rl'}) (s, e)
    | e < s' || s > e' = (Nothing, [(s, e)])
    | s' <= s && e <= e' = (Just (move s, move e), [])
    | s < s' && e <= e' = (Just (move s', move e), [(s, s' - 1)])
    | s >= s' && e > e' = (Just (move s, move e'), [(e' + 1, e)])
    | s < s' && e > e' = (Just (move s', move e'), [(s, s' - 1), (e' + 1, e)])
  where
    e' = s' + rl' - 1
    move i = drs' + (i - s')

mapToDestination' :: [FieldMap] -> Range -> [Range]
mapToDestination' [] s = [s]
mapToDestination' (fm : fms) s = maybeToList dst ++ concatMap (mapToDestination' fms) rest
  where
    (dst, rest) = sourceToDestination' fm s

fieldToDestination' :: [[FieldMap]] -> Range -> [Range]
fieldToDestination' fs s =
    foldl
        ( \rs fms ->
            concatMap
                (mapToDestination' fms)
                rs
        )
        [s]
        fs

solution2 :: Field -> Int
solution2 f = (minimum . fmap fst . concatMap (fieldToDestination' fs)) seeds'
  where
    fs = fieldMaps f
    seeds' = (fmap (\[s, r] -> (s, s + r)) . chunksOf 2 . seeds) f

fifthDecemberSolution2 :: IO Int
fifthDecemberSolution2 = solution2 <$> input

testInput :: Field
testInput =
    parseInput
        "seeds: 79 14 55 13\n\
        \\n\
        \seed-to-soil map:\n\
        \50 98 2\n\
        \52 50 48\n\
        \\n\
        \soil-to-fertilizer map:\n\
        \0 15 37\n\
        \37 52 2\n\
        \39 0 15\n\
        \\n\
        \fertilizer-to-water map:\n\
        \49 53 8\n\
        \0 11 42\n\
        \42 0 7\n\
        \57 7 4\n\
        \\n\
        \water-to-light map:\n\
        \88 18 7\n\
        \18 25 70\n\
        \\n\
        \light-to-temperature map:\n\
        \45 77 23\n\
        \81 45 19\n\
        \68 64 13\n\
        \\n\
        \temperature-to-humidity map:\n\
        \0 69 1\n\
        \1 0 69\n\
        \\n\
        \humidity-to-location map:\n\
        \60 56 37\n\
        \56 93 4"

fieldMaps :: Field -> [[FieldMap]]
fieldMaps f =
    [ seedToSoil f
    , soilToFertilizer f
    , fertilizerToWater f
    , waterToLight f
    , lightToTemperature f
    , temperatureToHumidity f
    , humitidyToLocation f
    ]

parseInput :: String -> Field
parseInput s =
    F
        { seeds = ss
        , seedToSoil = head fms
        , soilToFertilizer = fms !! 1
        , fertilizerToWater = fms !! 2
        , waterToLight = fms !! 3
        , lightToTemperature = fms !! 4
        , temperatureToHumidity = fms !! 5
        , humitidyToLocation = fms !! 6
        }
  where
    rawLists = (groupBy (\_ b -> b /= [""]) . fmap (splitOn "\n") . lines) s
    listToFieldMap l =
        FM
            { drs = read (head l) :: Int
            , srs = read (l !! 1) :: Int
            , rl = read (l !! 2) :: Int
            }
    ss = (fmap (\x -> read x :: Int) . tail . words . head . head . head) rawLists
    fms = (fmap (fmap (listToFieldMap . words . head) . drop 2) . tail) rawLists
