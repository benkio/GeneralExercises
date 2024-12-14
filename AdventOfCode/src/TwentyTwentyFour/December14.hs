{-# LANGUAGE OverloadedStrings #-}

module TwentyTwentyFour.December14 where

import Lib.Coord (Coord, cardinalNeighboors, findCardinalNeighboors)

import Data.Text hiding (filter, foldl, length)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal, signed)

data Robot = R
    { pos :: Coord
    , vx :: Int
    , vy :: Int
    }
    deriving (Show)

input :: IO [Robot]
input = parseInput . pack <$> readFile "input/2024/December14.txt"

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
    parseInput . pack $
        "p=0,4 v=3,-3\n\
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

robotPositionAtT :: Int -> (Int, Int) -> Robot -> Coord
robotPositionAtT t (bx, by) (R{pos = (px, py), vx = vX, vy = vY}) =
    (robotPositionAtT' px vX bx, robotPositionAtT' py vY by)
  where
    robotPositionAtT' p v b = (p + v * t) `mod` b

quadrants :: (Int, Int) -> [(Coord, Coord)]
quadrants (x, y) =
    [ ((0, 0), (qx, qy))
    , ((qx + 2, 0), (qx * 2 + 2, qy))
    , (((0, qy + 2)), (qx, qy * 2 + 2))
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

test = robotsInQuadrants [rm] (quadrants bsize)
  where
    bsize = (11, 7)
    r = R{pos = (2, 4), vx = 2, vy = (-3)}
    rm = r{pos = robotPositionAtT 4 bsize r}

test2 = fmap (\r -> r{pos = robotPositionAtT 100 (11, 7) r}) testInput

solution1 :: Int -> Coord -> [Robot] -> Int
solution1 t bSize = product . fmap length . (`robotsInQuadrants` (quadrants bSize)) . fmap (\r -> r{pos = robotPositionAtT t bSize r})

december14Solution1 :: IO Int
december14Solution1 = solution1 100 bathroomSize <$> input

solution2 :: a -> Int
solution2 = undefined

december14Solution2 :: IO Int
december14Solution2 = solution2 <$> input
