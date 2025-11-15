module TwentyTwentyThree.TwentyFourthDecember where

import Data.Bifunctor (bimap)
import Data.List (group, maximumBy, sort, tails)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow, traceShowId)

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
    , t2d_pz :: Double
    , t2d_vx :: Double
    , t2d_vy :: Double
    , t2d_vz :: Double
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

crossProduct :: (Num a) => (a, a, a) -> (a, a, a) -> (a, a, a)
crossProduct (a1, a2, a3) (b1, b2, b3) =
    (subtract (a2 * b3) (b2 * a3), subtract (b1 * a3) (a1 * b3), subtract (a1 * b2) (b1 * a2))

dotProduct :: (Num a) => (a, a, a) -> (a, a, a) -> a
dotProduct (a1, a2, a3) (b1, b2, b3) =
    a1 * b1 + a2 * b2 + a3 * b3

vectorApplyF :: (Num a) => (a, a, a) -> (a, a, a) -> (a -> a -> a) -> (a, a, a)
vectorApplyF (a1, a2, a3) (b1, b2, b3) f = (a1 `f` b1, a2 `f` b2, a3 `f` b3)

vectorApplyF' :: (Num a) => (a, a, a) -> a -> (a -> a -> a) -> (a, a, a)
vectorApplyF' (a1, a2, a3) x f = (a1 `f` x, a2 `f` x, a3 `f` x)

computeSolution :: Trajectory2D -> Trajectory2D -> Trajectory2D -> PointVelocity
computeSolution tr0 tr1 tr2 =
    PV
        { px = fpx
        , py = fpy
        , pz = fpz
        , vx = fvx
        , vy = fvy
        , vz = fvz
        }
  where
    position0 = (t2d_px tr0, t2d_py tr0, t2d_pz tr0)
    velocity0 = (t2d_vx tr0, t2d_vy tr0, t2d_vz tr0)
    position1 = (t2d_px tr1, t2d_py tr1, t2d_pz tr1)
    velocity1 = (t2d_vx tr1, t2d_vy tr1, t2d_vz tr1)
    position2 = (t2d_px tr2, t2d_py tr2, t2d_pz tr2)
    velocity2 = (t2d_vx tr2, t2d_vy tr2, t2d_vz tr2)
    p1 = vectorApplyF position1 position0 (-)
    v1 = vectorApplyF velocity1 velocity0 (-)
    p2 = vectorApplyF position2 position0 (-)
    v2 = vectorApplyF velocity2 velocity0 (-)
    t1 = -((p1 `crossProduct` p2) `dotProduct` v2) / ((v1 `crossProduct` p2) `dotProduct` v2)
    t2 = -((p1 `crossProduct` p2) `dotProduct` v1) / ((p1 `crossProduct` v2) `dotProduct` v1)
    c1 = vectorApplyF position1 (vectorApplyF' velocity1 t1 (*)) (+)
    c2 = vectorApplyF position2 (vectorApplyF' velocity2 t2 (*)) (+)
    (fvx, fvy, fvz) = vectorApplyF' (vectorApplyF c2 c1 (-)) (t2 - t1) (/)
    (fpx, fpy, fpz) = vectorApplyF c1 (vectorApplyF' (fvx, fvy, fvz) t1 (*)) (-)

solution2 = head . maximumBy (\l l' -> length l `compare` length l') . group . sort . fmap (\(x, y, z) -> round ((\p -> px p + py p + pz p) (computeSolution x y z))) . (\x -> zip3 x (tail x) (tail (tail x))) . fmap toTrajectory2d

-- 792453925505022 too low
-- 811662715630199 too low
-- 856642398547748 right
-- 1018285678400640 too high
twentyfourthDecemberSolution2 :: IO Int
twentyfourthDecemberSolution2 = solution2 <$> input

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
toTrajectory2d PV{px = x, py = y, pz = z, vx = vx', vy = vy', vz = vz'} =
    T2D
        { t2d_a = vy' / vx'
        , t2d_b = y - (vy' / vx') * x
        , t2d_px = x
        , t2d_py = y
        , t2d_pz = z
        , t2d_vx = vx'
        , t2d_vy = vy'
        , t2d_vz = vz'
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
