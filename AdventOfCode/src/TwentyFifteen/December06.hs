{-# LANGUAGE TupleSections #-}

module TwentyFifteen.December06 where

import Control.Parallel.Strategies
import Data.Maybe (isJust)
import Data.Map (Map)
import qualified Data.Map as M
import Lib.Coord
import Lib.List (diffMap)
import Lib.Parse

validInstructions :: [String]
validInstructions =
    [ "toggle"
    , "turn on"
    , "turn off"
    ]

input :: IO [Instruction]
input = parseInput <$> readFile "input/2015/6December.txt"

parseInput :: String -> [Instruction]
parseInput = parseInstructionsStartEnd validInstructions

type Instruction = (String, Coord, Coord)
type LightBlock = (Coord, Coord)

applyInstructions :: [LightBlock] -> [Instruction] -> [LightBlock]
applyInstructions [] (("turn on", sc, ec) : is) = applyInstructions [(sc, ec)] is
applyInstructions [] (("toggle", sc, ec) : is) = applyInstructions [(sc, ec)] is
applyInstructions [] (_ : is) = applyInstructions [] is
applyInstructions ls [] = ls
applyInstructions ls (("turn on", sc, ec) : is) = applyInstructions ((sc, ec) : (concatMap (subtractRect (sc, ec)) ls)) is
applyInstructions ls (("turn off", sc, ec) : is) = applyInstructions (concatMap (subtractRect (sc, ec)) ls) is
applyInstructions ls (("toggle", sc, ec) : is) =
    let
        currentLightOn = concatMap (subtractRect (sc, ec)) ls
        toggledBlocks = foldl (\lights dark -> concatMap (subtractRect dark) lights) [(sc, ec)] ls
        newLights = currentLightOn ++ toggledBlocks
     in
        applyInstructions newLights is

solution1 :: [Instruction] -> Int
solution1 = sum . fmap coordsRect . applyInstructions []

testInput :: [Instruction]
testInput =
    parseInput
        "turn on 0,0 through 999,999\n\
        \toggle 0,0 through 999,0\n\
        \turn off 499,499 through 500,500\n"

test = solution1 testInput

december06Solution1 :: IO Int
december06Solution1 = solution1 <$> input

-- Normalize rectangle coordinates
normalizeRect :: (Coord, Coord) -> (Coord, Coord)
normalizeRect ((x1, y1), (x2, y2)) = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

-- Apply brightness changes to all coordinates in a rectangle
updateBrightness :: Map Coord Int -> (Coord, Coord) -> Int -> Map Coord Int
updateBrightness brightnessMap rect delta =
    let ((minX, minY), (maxX, maxY)) = normalizeRect rect
        coords = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
     in foldl (\acc coord -> M.insertWith (+) coord delta acc) brightnessMap coords

applyInstructions' :: Map Coord Int -> [Instruction] -> Map Coord Int
applyInstructions' brightnessMap [] = brightnessMap
applyInstructions' brightnessMap (("turn on", sc, ec) : is) =
    applyInstructions' (updateBrightness brightnessMap (sc, ec) 1) is
applyInstructions' brightnessMap (("turn off", sc, ec) : is) =
    applyInstructions' (updateBrightnessDecrease brightnessMap (sc, ec)) is
applyInstructions' brightnessMap (("toggle", sc, ec) : is) =
    applyInstructions' (updateBrightness brightnessMap (sc, ec) 2) is

-- Decrease brightness by 1 (minimum 0) for all coordinates in rectangle
updateBrightnessDecrease :: Map Coord Int -> (Coord, Coord) -> Map Coord Int
updateBrightnessDecrease brightnessMap rect =
    let
      ((minX, minY), (maxX, maxY)) = normalizeRect rect
      coords = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]
     in foldl (\acc coord -> M.alter (Just . max 0 . maybe 0 (subtract 1)) coord acc) brightnessMap coords
  where

solution2 :: [Instruction] -> Int
solution2 = sum . M.elems . applyInstructions' M.empty

testInput2 =
    parseInput
        "turn on 0,0 through 9,9\n\
        \turn off 5,5 through 6,6\n"
test2 = solution2 testInput2

december06Solution2 :: IO Int
december06Solution2 = solution2 <$> input
