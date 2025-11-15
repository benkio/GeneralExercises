{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module TwentyTwentyFour.December14 where

import Data.List (group, nub, sortBy, sortOn)
import Data.Ord (comparing)
import Data.Text (Text, pack)
import Data.Void
import Debug.Trace
import Lib.Coord (Coord)
import Lib.Print (printGrid)
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Printf (printf)

data Robot = R
    { pos :: Coord
    , vx :: Int
    , vy :: Int
    }
    deriving (Show, Eq)

input :: IO [Robot]
input =
    parseInput
        . pack
        <$> readFile "input/2024/December14.txt"

parseInput :: Text -> [Robot]
parseInput t = either (error "parsing error") id $ runParser parser "" t
  where
    parser = many parseRobot

parseRobot :: Parsec Void Text Robot
parseRobot = do
    string "p="
    posX <- decimal
    string ","
    posY <- decimal
    string " v="
    velocityX <- signed space decimal
    string ","
    velocityY <- signed space decimal
    newline
    return $ R{pos = (posX, posY), vx = velocityX, vy = velocityY}

testInput :: [Robot]
testInput =
    parseInput
        . pack
        $ "p=0,4 v=3,-3\n\
          \p=6,3 v=-1,-3\n\
          \p=10,3 v=-1,2\n\
          \p=2,0 v=2,-1\n\
          \p=0,0 v=1,3\n\
          \p=3,0 v=-2,-2\n\
          \p=7,6 v=-1,-3\n\
          \p=3,0 v=-1,-2\n\
          \p=9,3 v=2,3\n\
          \p=7,3 v=-1,2\n\
          \p=2,4 v=2,-3\n\
          \p=9,5 v=-3,-3\n"

bathroomSize :: (Int, Int)
bathroomSize = (101, 103)

bathroomSizeTest :: (Int, Int)
bathroomSizeTest = (11, 7)

robotPositionAtT :: Int -> (Int, Int) -> Robot -> Coord
robotPositionAtT t (bx, by) (R{pos = (px, py), vx = vX, vy = vY}) =
    (robotPositionAtT' px vX bx, robotPositionAtT' py vY by)
  where
    robotPositionAtT' p v b = (p + v * t) `mod` b

moveRobot :: (Int, Int) -> Robot -> Robot
moveRobot (bx, by) (R{pos = (px, py), vx = vX, vy = vY}) =
    (R{pos = (robotPositionAtT' px vX bx, robotPositionAtT' py vY by), vx = vX, vy = vY})
  where
    robotPositionAtT' p v b = (p + v) `mod` b

moveRobots :: (Int, Int) -> [Robot] -> [Robot]
moveRobots bSize = fmap (moveRobot bSize)

quadrants :: (Int, Int) -> [(Coord, Coord)]
quadrants (x, y) =
    [ ((0, 0), (qx, qy))
    , ((qx + 2, 0), (qx * 2 + 2, qy))
    , ((0, qy + 2), (qx, qy * 2 + 2))
    , ((qx + 2, qy + 2), (qx * 2 + 2, qy * 2 + 2))
    ]
  where
    qx = (x - 1) `div` 2 - 1
    qy = (y - 1) `div` 2 - 1

robotsInQuadrant :: (Coord, Coord) -> [Robot] -> [Robot]
robotsInQuadrant ((plx, ply), (phx, phy)) =
    filter
        ( ( \(x, y) ->
                x <= phx
                    && x >= plx
                    && y <= phy
                    && y >= ply
          )
            . pos
        )

robotsInQuadrants :: [Robot] -> [(Coord, Coord)] -> [[Robot]]
robotsInQuadrants rs = foldl (\acc q -> robotsInQuadrant q rs : acc) []

evolveToT :: Int -> Coord -> [Robot] -> [Robot]
evolveToT t bSize =
    fmap (\r -> r{pos = robotPositionAtT t bSize r})

solution1 :: Int -> Coord -> [Robot] -> Int
solution1 t bSize =
    product
        . fmap length
        . (`robotsInQuadrants` quadrants bSize)
        . evolveToT t bSize

printRobots :: Coord -> [Robot] -> String
printRobots bSize =
    printGrid bSize (const "R")
        . fmap pos

december14Solution1 :: IO Int
december14Solution1 = solution1 100 bathroomSize <$> input

-- loop testInput: 77
-- loop input: 10403
loop :: [Robot] -> Coord -> Int
loop rs bSize = go 1 (moveRobots bSize rs)
  where
    go !n rs' = if rs == rs' then n else go (n + 1) (moveRobots bSize rs')

atLeastNAdjacentRobotsByDirection :: Int -> Coord -> [Robot] -> Bool
atLeastNAdjacentRobotsByDirection n (bx, by) rs =
    not (null consecutiveDistances)
        && ((>= n) . length . last) consecutiveDistances
  where
    psRow = (sortBy (comparing snd <> comparing fst) . fmap pos) rs
    psCol = (sortBy (comparing fst <> comparing snd) . fmap pos) rs
    distancesX xs = zipWith (\(x, y) (a, b) -> abs (x - a) + (abs (y - b) * bx)) xs (tail xs)
    distancesY xs = zipWith (\(x, y) (a, b) -> abs (y - b) + (abs (x - a) * by)) xs (tail xs)
    allDistances = distancesY psCol ++ distancesX psRow
    consecutiveDistances = sortOn length . filter (all (== 1)) . group $ allDistances

findByAdjacentRobots :: Int -> Int -> Coord -> [Robot] -> Int
findByAdjacentRobots adjacents nMax bSize rs = go 1 (moveRobots bSize rs)
  where
    go !n rs' = if atLeastNAdjacentRobotsByDirection adjacents bSize rs' || n >= nMax then n else go (traceShowId (n + 1)) (moveRobots bSize rs')

solution2 :: [Robot] -> Int
solution2 xs = findByAdjacentRobots 10 loopValue bathroomSize xs
  where
    loopValue = loop xs bathroomSize

-- 7774
december14Solution2 :: IO Int
december14Solution2 = solution2 <$> input

december14Solution2Print :: IO ()
december14Solution2Print = (\i -> putStrLn (printRobots bathroomSize (evolveToT (solution2 i) bathroomSize i))) =<< input
