module TwentyTwentyThree.TwentyFourthDecember where

import Debug.Trace (traceShow, traceShowId)

import Data.List.Split (splitOn)

import Data.Bifunctor (bimap)

data PointVelocity = PV
    { px :: Double
    , py :: Double
    , pz :: Double
    , vx :: Double
    , vy :: Double
    , vz :: Double
    }
    deriving (Show)

input :: IO [PointVelocity]
input = parseInput <$> readFile "input/2023/24December.txt"

solution1 = undefined

twentyfourthDecemberSolution1 :: IO Int
twentyfourthDecemberSolution1 = undefined

solution2 = undefined

twentyfourthDecemberSolution2 :: IO Int
twentyfourthDecemberSolution2 = undefined

testAreaLow :: Int
testAreaLow = 200000000000000
testAreaHigh :: Int
testAreaHigh = 400000000000000

inTestArea :: (Int,Int) -> Bool
inTestArea (x,y) =
  x >= testAreaLow && x <= testAreaHigh &&
  y >= testAreaLow && y <= testAreaHigh

-- pointOfCollision :: PointVelocity -> PointVelocity -> (Double, Double)
-- pointOfCollision PV{px = x1, py = y1, vx = vx1, vy = vy1} PV{px = x2, py = y2, vx = vx2, vy = vy2} =
--   let
--     t = traceShowId $ (x1 - x2) / (vx2 - vx1)
--     t' = traceShowId $ (y1 - y2) / (vy2 - vy1)
--     x = t'*vx1 + x1
--     y = t'*vy1 + y1
--   in (x,y)

-- areParallel :: PointVelocity -> PointVelocity -> Bool
-- areParallel PV{px = x1, py = y1, vx = vx1, vy = vy1} PV{px = x2, py = y2, vx = vx2, vy = vy2} =
--     x1 /= x2
--         && y1 /= y2
--         && vx1 == vx2
--         && vy1 == vy2

-- areCoincident :: PointVelocity -> PointVelocity -> Bool
-- areCoincident PV{px = x1, py = y1, vx = vx1, vy = vy1} PV{px = x2, py = y2, vx = vx2, vy = vy2} =
--     x1 == x2
--         && y1 == y2
--         && vx1 == vx2
--         && vy1 == vy2

parseInput :: String -> [PointVelocity]
parseInput =
    fmap
        ( listsToPointVelocity
            . bimap
                (fmap (\x -> read x :: Double) . splitOn ", ")
                (fmap (\x -> read x :: Double) . splitOn ", " . drop 2)
            . break (== '@')
        )
        . lines
  where
    listsToPointVelocity :: ([Double], [Double]) -> PointVelocity
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
