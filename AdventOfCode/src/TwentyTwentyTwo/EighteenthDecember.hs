module TwentyTwentyTwo.EighteenthDecember where

import Data.List (group, sort)

type Obsidian = (Int, Int, Int)
type ObsidianFace = (Float, Float, Float)

parseInput = fmap parseObsidian . lines
  where
    parseX = takeWhile (/= ',')
    parseY = parseX . tail . dropWhile (/= ',')
    parseZ = parseY . tail . dropWhile (/= ',')
    parseObsidian s =
        ( read (parseX s) :: Int
        , read (parseY s) :: Int
        , read (parseZ s) :: Int
        )

input :: IO [Obsidian]
input = parseInput <$> readFile "input/2022/18December.txt"

testInput :: [Obsidian]
testInput =
    parseInput
        "2,2,2\n\
        \1,2,2\n\
        \3,2,2\n\
        \2,1,2\n\
        \2,3,2\n\
        \2,2,1\n\
        \2,2,3\n\
        \2,2,4\n\
        \2,2,6\n\
        \1,2,5\n\
        \3,2,5\n\
        \2,1,5\n\
        \2,3,5"

computeFaces :: Obsidian -> [ObsidianFace]
computeFaces (a, b, c) =
    [ (x + 0.5, y, z)
    , (x - 0.5, y, z)
    , (x, y + 0.5, z)
    , (x, y - 0.5, z)
    , (x, y, z + 0.5)
    , (x, y, z - 0.5)
    ]
  where
    x = fromIntegral a :: Float
    y = fromIntegral b :: Float
    z = fromIntegral c :: Float

solution1 :: [Obsidian] -> Int
solution1 = sum . fmap length . filter ((== 1) . length) . group . sort . concatMap computeFaces

eighteenthDecemberSolution1 :: IO Int
eighteenthDecemberSolution1 = solution1 <$> input

eighteenthDecemberSolution2 :: IO Int
eighteenthDecemberSolution2 = undefined
