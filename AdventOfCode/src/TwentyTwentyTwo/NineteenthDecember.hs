module TwentyTwentyTwo.NineteenthDecember where

import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import Text.Read

newtype OreRobotCost = OreRobotCost {oreCost :: Int} deriving (Show)
newtype ClayRobotCost = ClayRobotCost {clayOreCost :: Int} deriving (Show)
data ObsidianRobotCost = ObsidianRobotCost {obsidianOreCost :: Int, obsidianClayCost :: Int} deriving (Show)
data GeodeRobotCost = GeodeRobotCost {geodeOreCost :: Int, geodeObsidianCost :: Int} deriving (Show)
data Robot = Ore | Clay | Obsidian | Geode | NoRobot deriving (Show)

robots = [Ore, Clay, Obsidian, Geode, NoRobot]

data Blueprint = Blueprint
    { num :: Int
    , oreRobotCost :: OreRobotCost
    , clayRobotCost :: ClayRobotCost
    , obsidianRobotCost :: ObsidianRobotCost
    , geodeRobotCost :: GeodeRobotCost
    }
    deriving (Show)

data State = State
    { ore :: Int
    , clay :: Int
    , obsidian :: Int
    , geode :: Int
    , r_ore :: Int
    , r_clay :: Int
    , r_obsidian :: Int
    , r_geode :: Int
    }
    deriving (Show)

startingState =
    State
        { ore = 0
        , clay = 0
        , obsidian = 0
        , geode = 0
        , r_ore = 1
        , r_clay = 0
        , r_obsidian = 0
        , r_geode = 0
        }

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

testInput :: [Blueprint]
testInput =
    parseInput
        "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n\
        \Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."

collectingMaterial :: State -> State
collectingMaterial s =
    s
        { ore = ore s + r_ore s
        , clay = clay s + r_clay s
        , obsidian = obsidian s + r_obsidian s
        , geode = geode s + r_geode s
        }

spendResources :: State -> Blueprint -> [(State, Robot)]
spendResources s b = mapMaybe (buildRobot s b) robots

computeMinute :: State -> Blueprint -> [State]
computeMinute s b = uncurry addRobot . first collectingMaterial <$> spendResources s b

addRobot :: State -> Robot -> State
addRobot s Ore = s{r_ore = r_ore s + 1}
addRobot s Clay = s{r_clay = r_clay s + 1}
addRobot s Obsidian = s{r_obsidian = r_obsidian s + 1}
addRobot s Geode = s{r_geode = r_geode s + 1}
addRobot s NoRobot = s

buildRobot :: State -> Blueprint -> Robot -> Maybe (State, Robot)
buildRobot s _ NoRobot = Just (s, NoRobot)
buildRobot s b Ore = if ore s >= (oreCost . oreRobotCost) b then Just ((s{ore = ore s - (oreCost . oreRobotCost) b}, Ore)) else Nothing
buildRobot s b Clay = if ore s >= (clayOreCost . clayRobotCost) b then Just ((s{ore = ore s - (clayOreCost . clayRobotCost) b}, Clay)) else Nothing
buildRobot s b Obsidian =
    if ore s >= (obsidianOreCost . obsidianRobotCost) b && clay s >= (obsidianClayCost . obsidianRobotCost) b
        then
            Just
                ( ( s
                        { ore = ore s - (obsidianOreCost . obsidianRobotCost) b
                        , clay = clay s - (obsidianClayCost . obsidianRobotCost) b
                        }
                  , Obsidian
                  )
                )
        else Nothing
buildRobot s b Geode =
    if ore s >= (geodeOreCost . geodeRobotCost) b && obsidian s >= (geodeObsidianCost . geodeRobotCost) b
        then
            Just
                ( ( s
                        { ore = ore s - (geodeOreCost . geodeRobotCost) b
                        , obsidian = obsidian s - (geodeObsidianCost . geodeRobotCost) b
                        }
                  , Geode
                  )
                )
        else Nothing

test = (computeMinute startingState . head) testInput

nineteenthDecemberSolution1 :: IO Int
nineteenthDecemberSolution1 = undefined

nineteenthDecemberSolution2 :: IO Int
nineteenthDecemberSolution2 = undefined
