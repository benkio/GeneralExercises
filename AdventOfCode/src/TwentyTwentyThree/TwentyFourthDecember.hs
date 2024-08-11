module TwentyTwentyThree.TwentyFourthDecember where

import Debug.Trace (traceShow, traceShowId)

import Data.Bifunctor (bimap)
import Data.List (tails)
import Data.List.Split (splitOn)

data PointVelocity = PV
    { px :: Double
    , py :: Double
    , pz :: Double
    , vx :: Double
    , vy :: Double
    , vz :: Double
    }
    deriving (Show)

-- y = ax + b
data Trajectory2D = T2D
    { t2d_a :: Double
    , t2d_b :: Double
    , t2d_px :: Double
    , t2d_py :: Double
    , t2d_vx :: Double
    , t2d_vy :: Double
    }
    deriving (Show)

input :: IO [PointVelocity]
input = parseInput <$> readFile "input/2023/24December.txt"

solution1 =
    length
        . filter (\(l1, l2, cp) -> inTestArea cp && not (inThePast l1 cp) && not (inThePast l2 cp))
        . fmap (\(l1, l2) -> (l1, l2, collisionPoint l1 l2))
        . filter (\(l1, l2) -> not (areParallel l1 l2 || areCoincident l1 l2))
        . buildPairs
        . fmap toTrajectory2d

twentyfourthDecemberSolution1 :: IO Int
twentyfourthDecemberSolution1 = solution1 <$> input

solution2 = undefined

twentyfourthDecemberSolution2 :: IO Int
twentyfourthDecemberSolution2 = undefined

buildPairs :: [a] -> [(a, a)]
buildPairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

testAreaLow :: Double
testAreaLow = 200000000000000
testAreaHigh :: Double
testAreaHigh = 400000000000000

inTestArea :: (Double, Double) -> Bool
inTestArea (x, y) =
    x >= testAreaLow
        && x <= testAreaHigh
        && y >= testAreaLow
        && y <= testAreaHigh

inThePast :: Trajectory2D -> (Double, Double) -> Bool
inThePast T2D{t2d_vx = vx, t2d_vy = vy, t2d_px = x1, t2d_py = y1} (x2, y2) =
    (vx > 0 && x2 < x1) || (vx < 0 && x2 > x1) || (vx == 0 && (vy > 0 && y2 < y1)) && (vx == 0 && (vy < 0 && y2 > y1))

toTrajectory2d :: PointVelocity -> Trajectory2D
toTrajectory2d PV{px = x, py = y, vx = vx', vy = vy'} =
    T2D
        { t2d_a = vy' / vx'
        , t2d_b = y - (vy' / vx') * x
        , t2d_px = x
        , t2d_py = y
        , t2d_vx = vx'
        , t2d_vy = vy'
        }

areParallel :: Trajectory2D -> Trajectory2D -> Bool
areParallel T2D{t2d_a = a1} T2D{t2d_a = a2} = a1 == a2
areCoincident :: Trajectory2D -> Trajectory2D -> Bool
areCoincident T2D{t2d_a = a1, t2d_b = b1} T2D{t2d_a = a2, t2d_b = b2} = a1 == a2 && b1 == b2
collisionPoint :: Trajectory2D -> Trajectory2D -> (Double, Double)
collisionPoint T2D{t2d_a = a1, t2d_b = b1} T2D{t2d_a = a2, t2d_b = b2} =
    ( (b2 - b1) / (a1 - a2)
    , ((b2 - b1) / (a1 - a2)) * a1 + b1
    )

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
