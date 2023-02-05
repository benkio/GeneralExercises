module TwentyTwentyTwo.NineteenthDecember where

import Data.Bifunctor (first)
import Data.List (find)
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Set (Set, notMember)
import qualified Data.Set as Set
import Debug.Trace
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

maximumResource :: Blueprint -> Robot -> Int
maximumResource b Ore = maximum [(oreCost . oreRobotCost) b, (clayOreCost . clayRobotCost) b, (obsidianOreCost . obsidianRobotCost) b, (geodeOreCost . geodeRobotCost) b]
maximumResource b Clay = (obsidianClayCost . obsidianRobotCost) b
maximumResource b Obsidian = (geodeObsidianCost . geodeRobotCost) b

enoughResourcesMined :: State -> Blueprint -> Robot -> Int -> Bool
enoughResourcesMined s b Ore tl = (r_ore s * tl) + ore s >= tl * (maximumResource b Ore)
enoughResourcesMined s b Clay tl = (r_clay s * tl) + clay s >= tl * (maximumResource b Clay)
enoughResourcesMined s b Obsidian tl = (r_obsidian s * tl) + obsidian s >= tl * (maximumResource b Obsidian)

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
    deriving (Eq, Ord, Show)

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

spendResources :: State -> Blueprint -> Int -> [(State, Robot)]
spendResources s b tl = mapMaybe (\r -> buildRobot s b r tl) robots

computeMinute :: State -> Blueprint -> Int -> [State]
computeMinute s b tl = uncurry addRobot . first collectingMaterial <$> spendResources s b tl

addRobot :: State -> Robot -> State
addRobot s Ore = s{r_ore = r_ore s + 1}
addRobot s Clay = s{r_clay = r_clay s + 1}
addRobot s Obsidian = s{r_obsidian = r_obsidian s + 1}
addRobot s Geode = s{r_geode = r_geode s + 1}
addRobot s NoRobot = s

enoughRobots :: State -> Blueprint -> Robot -> Bool
enoughRobots s b Ore = r_ore s == (oreCost . oreRobotCost) b
enoughRobots s b Clay = r_ore s == (clayOreCost . clayRobotCost) b
enoughRobots s b Obsidian = r_ore s == (obsidianOreCost . obsidianRobotCost) b && r_clay s == (obsidianClayCost . obsidianRobotCost) b
enoughRobots s b Geode = r_ore s == (geodeOreCost . geodeRobotCost) b && r_obsidian s == (geodeObsidianCost . geodeRobotCost) b
enoughRobots s b NoRobot = False --I always consider valid doing nothing

enoughResources :: State -> Blueprint -> Robot -> Bool
enoughResources s b Ore = ore s >= (oreCost . oreRobotCost) b
enoughResources s b Clay = ore s >= (clayOreCost . clayRobotCost) b
enoughResources s b Obsidian = ore s >= (obsidianOreCost . obsidianRobotCost) b && clay s >= (obsidianClayCost . obsidianRobotCost) b
enoughResources s b Geode = ore s >= (geodeOreCost . geodeRobotCost) b && obsidian s >= (geodeObsidianCost . geodeRobotCost) b
enoughResources s b NoRobot = True

buildRobot :: State -> Blueprint -> Robot -> Int -> Maybe (State, Robot)
buildRobot s _ NoRobot _ = Just (s, NoRobot)
buildRobot s b Ore tl =
    if enoughResources s b Ore && not (enoughRobots s b Ore) && not (enoughResourcesMined s b Ore tl)
        then Just ((s{ore = ore s - (oreCost . oreRobotCost) b}, Ore))
        else Nothing
buildRobot s b Clay tl =
    if enoughResources s b Clay && not (enoughRobots s b Clay) && not (enoughResourcesMined s b Clay tl)
        then Just ((s{ore = ore s - (clayOreCost . clayRobotCost) b}, Clay))
        else Nothing
buildRobot s b Obsidian tl =
    if enoughResources s b Obsidian && not (enoughRobots s b Obsidian) && not (enoughResourcesMined s b Obsidian tl)
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
buildRobot s b Geode tl =
    if enoughResources s b Geode && not (enoughRobots s b Geode)
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

totalTime :: Int
totalTime = 24

timeLeft :: Int -> Int
timeLeft i = totalTime - i

search :: [State] -> Blueprint -> Set State -> Int -> [State]
search [] _ _ _ = error "unexpected empty states"
search sts b visited i
    | i == totalTime = sts -- maxGeode
    | otherwise = search sts'' b visited' (i + 1)
  where
    --maxGeode =  maximum $ fmap geode sts
    sts' =
        filter
            ( \s -> s `notMember` visited -- && geode s == maxGeode
            )
            sts
    sts'' = concatMap (\s -> computeMinute s b (timeLeft i)) sts'
    visited' = foldl (\acc s -> Set.insert s acc) visited sts'

test = maximum . fmap geode . (\b -> search [startingState] b Set.empty 0) $ testInput !! 1

nineteenthDecemberSolution1 :: IO Int
nineteenthDecemberSolution1 = undefined

nineteenthDecemberSolution2 :: IO Int
nineteenthDecemberSolution2 = undefined
