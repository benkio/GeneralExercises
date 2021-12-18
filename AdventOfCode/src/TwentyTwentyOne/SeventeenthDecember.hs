module TwentyTwentyOne.SeventeenthDecember where

import Data.List (maximumBy, unfoldr)

data Probe = Probe
  { positionX :: Int,
    positionY :: Int,
    velocityX :: Int,
    velocityY :: Int
  }
  deriving (Show)

data Area = Area
  { minX :: Int,
    maxX :: Int,
    minY :: Int,
    maxY :: Int
  }
  deriving (Show)

parseInput :: String -> Area
parseInput s =
  let minX' = ((\x -> read x :: Int) . takeWhile (/= '.') . tail . dropWhile (/= '=')) s
      maxX' = ((\x -> read x :: Int) . takeWhile (/= ',') . drop 2 . dropWhile (/= '.')) s
      minY' = ((\x -> read x :: Int) . takeWhile (/= '.') . tail . dropWhile (/= '=') . tail . dropWhile (/= '=')) s
      maxY' = ((\x -> read x :: Int) . takeWhile (/= ',') . drop 2 . dropWhile (/= '.') . tail . dropWhile (/= '=') . tail . dropWhile (/= '=')) s
   in Area {minX = minX', maxX = maxX', minY = minY', maxY = maxY'}

nextStep :: Probe -> Probe
nextStep Probe {positionX = px, positionY = py, velocityX = vx, velocityY = vy} =
  Probe {positionX = px + vx, positionY = py + vy, velocityX = applyDrag vx, velocityY = vy - 1}

applyDrag :: Int -> Int
applyDrag x
  | x > 0 = x - 1
  | x < 0 = x + 1
  | otherwise = 0

generateTrajectory :: Probe -> Area -> [Probe]
generateTrajectory p Area {minY = my, maxX = mx} = unfoldr (nextStepYBound my mx) p
  where
    nextStepYBound :: Int -> Int -> Probe -> Maybe (Probe, Probe)
    nextStepYBound y x pr = let pr' = nextStep pr in if positionY pr < y || positionX pr > x then Nothing else Just (pr, pr')

probeInArea :: Probe -> Area -> Bool
probeInArea Probe {positionX = px, positionY = py} a = (px, py) `elem` areaToCoords a

isHitTrajectory :: [Probe] -> Area -> Bool
isHitTrajectory ps = probeInArea (last ps)

areaToCoords :: Area -> [(Int, Int)]
areaToCoords Area {minX = minX', maxX = maxX', minY = minY', maxY = maxY'} =
  [(x, y) | x <- [minX' .. maxX'], y <- [minY' .. maxY']]

higherYInTrajectory :: [Probe] -> Int
higherYInTrajectory = foldl (\acc p -> max (positionY p) acc) 0

solution1 :: Area -> Int
solution1 a =
  let candidateVelocities = [(x, y) | x <- [1 .. maxX a], y <- [(abs . maxY) a .. (abs . minY) a]]
      initialProbes = fmap (\(vx, vy) -> Probe {positionX = 0, positionY = 0, velocityX = vx, velocityY = vy}) candidateVelocities
      hitTrajectories = (filter (`isHitTrajectory` a) . fmap (`generateTrajectory` a)) initialProbes
      highestTajectory =
        maximumBy (\(y, _) (y', _) -> y `compare` y') $
          fmap
            ( \ps ->
                let y = higherYInTrajectory ps
                    initialVelocity = ((\p -> (velocityX p, velocityY p)) . head) ps
                 in (y, initialVelocity)
            )
            hitTrajectories
   in fst highestTajectory

input :: IO String
input = readFile "input/2021/17December.txt"

inputTest :: String
inputTest = "target area: x=20..30, y=-10..-5"

seventeenthDecemberSolution1 :: IO Int
seventeenthDecemberSolution1 = solution1 . parseInput <$> input

solution2 :: Area -> [(Int, Int)]
solution2 a =
  let candidateVelocities = [(x, y) | x <- [1 .. maxX a], y <- [minY a .. (abs . minY) a]] -- more cleaver initial velocity?
      initialProbes = fmap (\(vx, vy) -> Probe {positionX = 0, positionY = 0, velocityX = vx, velocityY = vy}) candidateVelocities
      hitTrajectories = (filter (`isHitTrajectory` a) . fmap (`generateTrajectory` a)) initialProbes
   in fmap ((\p -> (velocityX p, velocityY p)) . head) hitTrajectories

seventeenthDecemberSolution2 :: IO Int
seventeenthDecemberSolution2 = length . solution2 . parseInput <$> input
