module TwentyTwentyThree.TwentyFourthDecember where

import Data.List.Split (splitOn)

import Data.Bifunctor (bimap)

data PointVelocity = PV
    { px :: Int
    , py :: Int
    , pz :: Int
    , vx :: Int
    , vy :: Int
    , vz :: Int
    }
    deriving (Show)

input :: IO [PointVelocity]
input = parseInput <$> readFile "input/2023/24December.txt"

parseInput :: String -> [PointVelocity]
parseInput =
    fmap
        ( listsToPointVelocity
            . bimap
                (fmap (\x -> read x :: Int) . splitOn ", ")
                (fmap (\x -> read x :: Int) . splitOn ", " . drop 2)
            . break (== '@')
        )
        . lines
  where
    listsToPointVelocity :: ([Int], [Int]) -> PointVelocity
    listsToPointVelocity ([x, y, z], [vx', vy', vz']) =
        PV
            { px = x
            , py = y
            , pz = z
            , vx = vx'
            , vy = vy'
            , vz = vz'
            }

testInput :: [PointVelocity]
testInput =
    parseInput
        "19, 13, 30 @ -2,  1, -2\n\
        \18, 19, 22 @ -1, -1, -2\n\
        \20, 25, 34 @ -2, -2, -4\n\
        \12, 31, 28 @ -1, -2, -1\n\
        \20, 19, 15 @  1, -5, -3"

solution1 = undefined

twentyfourthDecemberSolution1 :: IO Int
twentyfourthDecemberSolution1 = undefined

solution2 = undefined

twentyfourthDecemberSolution2 :: IO Int
twentyfourthDecemberSolution2 = undefined
