module TwentyTwentyTwo.December19 where

import Control.Parallel.Strategies
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Text.Read

newtype OreRobotCost = OreRobotCost {oreCost :: Int} deriving (Show)

newtype ClayRobotCost = ClayRobotCost {clayOreCost :: Int} deriving (Show)

data ObsidianRobotCost = ObsidianRobotCost {obsidianOreCost :: Int, obsidianClayCost :: Int} deriving (Show)

data GeodeRobotCost = GeodeRobotCost {geodeOreCost :: Int, geodeObsidianCost :: Int} deriving (Show)

data Robot = Ore | Clay | Obsidian | Geode deriving (Show)

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
        }

robots = [Ore, Clay, Obsidian, Geode]

maxResource :: Robot -> Blueprint -> Int
maxResource Ore b =
    maximum
        [ (oreCost . oreRobotCost) b
        , (clayOreCost . clayRobotCost) b
        , (obsidianOreCost . obsidianRobotCost) b
        , (geodeOreCost . geodeRobotCost) b
        ]
maxResource Clay b = (obsidianClayCost . obsidianRobotCost) b
maxResource Obsidian b = (geodeObsidianCost . geodeRobotCost) b

enoughRobot :: State -> Blueprint -> Robot -> Bool
enoughRobot s b Ore = maxResource Ore b <= r_ore s
enoughRobot s b Clay = maxResource Clay b <= r_clay s
enoughRobot s b Obsidian = maxResource Obsidian b <= r_ore s
enoughRobot s b Geode = False

-- Note that we can do a bit better: For any resource R that's not
-- geode: if you already have X robots creating resource R, a current
-- stock of Y for that resource, T minutes left, and no robot requires
-- more than Z of resource R to build, and X * T+Y >= T * Z, then you
-- never need to build another robot mining R anymore.
enoughResource :: State -> Int -> Int -> Blueprint -> Robot -> Bool
enoughResource s time totalTime b Ore = r_ore s * (totalTime - time) + ore s >= (totalTime - time) * maxResource Ore b
enoughResource s time totalTime b Clay = r_clay s * (totalTime - time) + clay s >= (totalTime - time) * maxResource Clay b
enoughResource s time totalTime b Obsidian = r_obsidian s * (totalTime - time) + obsidian s >= (totalTime - time) * maxResource Obsidian b
enoughResource s time totalTime b Geode = False

evolveState :: Int -> State -> State
evolveState t s =
    s
        { ore = ore s + r_ore s * t
        , clay = clay s + r_clay s * t
        , obsidian = obsidian s + r_obsidian s * t
        -- , geode = geode s + r_geode s * t
        }

-- return the states with new robots and the time when those are operational
newRobotStates :: State -> Int -> Int -> Blueprint -> [(Int, State)]
newRobotStates s time totalTime b =
    ( filter ((<= totalTime) . fst)
        . mapMaybe (fmap (\(t, s) -> (t + time, s)) . newRobotState s time b totalTime)
        . filter (\r -> (not . enoughRobot s b) r && (not . enoughResource s time totalTime b) r) -- do I need to buy this bot
    )
        robots

newRobotState :: State -> Int -> Blueprint -> Int -> Robot -> Maybe (Int, State)
newRobotState s time b totalTime Ore =
    ( \t ->
        ( t
        , ((\s' -> s'{r_ore = r_ore s' + 1, ore = ore s' - (oreCost . oreRobotCost) b}) . evolveState t) s
        )
    )
        <$> newRobotTime s b Ore
newRobotState s time b totalTime Clay =
    ( \t ->
        ( t
        , ((\s' -> s'{r_clay = r_clay s' + 1, ore = ore s' - (clayOreCost . clayRobotCost) b}) . evolveState t) s
        )
    )
        <$> newRobotTime s b Clay
newRobotState s time b totalTime Obsidian =
    ( \t ->
        ( t
        , ( ( \s' ->
                s'
                    { r_obsidian = r_obsidian s' + 1
                    , ore = ore s' - (obsidianOreCost . obsidianRobotCost) b
                    , clay = clay s' - (obsidianClayCost . obsidianRobotCost) b
                    }
            )
                . evolveState t
          )
            s
        )
    )
        <$> newRobotTime s b Obsidian
newRobotState s time b totalTime Geode =
    ( \t ->
        ( t
        , ( ( \s' ->
                s'
                    { geode = geode s + (totalTime - (time + t))
                    , ore = ore s' - (geodeOreCost . geodeRobotCost) b
                    , obsidian = obsidian s' - (geodeObsidianCost . geodeRobotCost) b
                    }
            )
                . evolveState t
          )
            s
        )
    )
        <$> newRobotTime s b Geode

newRobotTime :: State -> Blueprint -> Robot -> Maybe Int
newRobotTime s b r = case r of
    Ore -> timeRequired ((oreCost . oreRobotCost) b) (ore s) (r_ore s)
    Clay -> timeRequired ((clayOreCost . clayRobotCost) b) (ore s) (r_ore s)
    Obsidian -> timeRequired ((obsidianOreCost . obsidianRobotCost) b) (ore s) (r_ore s) >>= \t -> fmap (max t) (timeRequired ((obsidianClayCost . obsidianRobotCost) b) (clay s) (r_clay s))
    Geode -> timeRequired ((geodeOreCost . geodeRobotCost) b) (ore s) (r_ore s) >>= \t -> fmap (max t) (timeRequired ((geodeObsidianCost . geodeRobotCost) b) (obsidian s) (r_obsidian s))
  where
    timeRequired :: Int -> Int -> Int -> Maybe Int
    timeRequired price resource robots
        | robots == 0 = Nothing
        | robots /= 0 && resource < price = (Just . (+ 1) . ceiling) $ fromIntegral (price - resource) / fromIntegral robots
        | otherwise = Just 1

bfs :: [(Int, State)] -> Set State -> Int -> Blueprint -> Int -> Int
bfs [] _ result _ _ = result
bfs ((t, s) : sts) visited result b totalTime = -- trace ((show . length) sts) $ 
    bfs (sts ++ nextStates) visited' (max (geode finalState) result) b totalTime
  where
    finalState = evolveState (totalTime - t) s
    nextStates = filter ((`Set.notMember` visited) . snd) $ newRobotStates s t totalTime b
    visited' = foldl (\acc (_, x) -> Set.insert x acc) visited nextStates

input :: IO [Blueprint]
input = parseInput <$> readFile "input/2022/19December.txt"

solution1 :: [Blueprint] -> Int
solution1 = sum . computeQualityLevels
  where
    computeQualityLevels = parMap rseq (\b -> num b * bfs [(0, startingState)] (Set.singleton startingState) 0 b 24)

december19Solution1 :: IO Int
december19Solution1 = solution1 <$> input

solution2 :: [Blueprint] -> Int
solution2 = product . parMap rseq (\b -> bfs [(0, startingState)] (Set.singleton startingState) 0 b 32)

december19Solution2 :: IO Int
december19Solution2 = solution2 . take 3 <$> input

parseInput :: String -> [Blueprint]
parseInput = fmap buildBlueprint . zip [1 ..] . lines
  where
    nums s = mapMaybe (\x -> readMaybe x :: Maybe Int) (words s)
    buildBlueprint (i, s) =
        let ns = nums s
         in Blueprint
                { num = i
                , oreRobotCost = OreRobotCost{oreCost = head ns}
                , clayRobotCost = ClayRobotCost{clayOreCost = ns !! 1}
                , obsidianRobotCost = ObsidianRobotCost{obsidianOreCost = ns !! 2, obsidianClayCost = ns !! 3}
                , geodeRobotCost = GeodeRobotCost{geodeOreCost = ns !! 4, geodeObsidianCost = ns !! 5}
                }

testInput :: [Blueprint]
testInput =
    parseInput
        "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n\
        \Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian."
