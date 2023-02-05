module TwentyTwentyTwo.NineteenthDecember where

import Data.Maybe (mapMaybe)
import Text.Read

newtype OreRobotCost = OreRobotCost {oreCost :: Int} deriving (Show)
newtype ClayRobotCost = ClayRobotCost {clayOreCost :: Int} deriving (Show)
data ObsidianRobotCost = ObsidianRobotCost {obsidianOreCost :: Int, obsidianClayCost :: Int} deriving (Show)
data GeodeRobotCost = GeodeRobotCost {geodeOreCost :: Int, geodeObsidianCost :: Int} deriving (Show)

data Blueprint = Blueprint
    { num :: Int
    , oreRobotCost :: OreRobotCost
    , clayRobotCost :: ClayRobotCost
    , obsidianRobotCost :: ObsidianRobotCost
    , geodeRobotCost :: GeodeRobotCost
    }
    deriving (Show)

input :: IO [Blueprint]
input = parseInput <$> readFile "input/2022/19December.txt"

parseInput :: String -> [Blueprint]
parseInput = fmap buildBlueprint . (zip [1 ..]) . lines
  where
    nums s = mapMaybe (\x -> readMaybe x :: Maybe Int) (words s)
    buildBlueprint (i, s) =
        let ns = nums s
         in Blueprint
                { num = i
                , oreRobotCost = OreRobotCost{oreCost = ns !! 0}
                , clayRobotCost = ClayRobotCost{clayOreCost = ns !! 1}
                , obsidianRobotCost = ObsidianRobotCost{obsidianOreCost = ns !! 2, obsidianClayCost = ns !! 3}
                , geodeRobotCost = GeodeRobotCost{geodeOreCost = ns !! 4, geodeObsidianCost = ns !! 5}
                }

testInput :: String
testInput =
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n\
    \Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."

nineteenthDecemberSolution1 :: IO Int
nineteenthDecemberSolution1 = undefined

nineteenthDecemberSolution2 :: IO Int
nineteenthDecemberSolution2 = undefined
