module TwentyFifteen.December06 where

import Control.Parallel.Strategies
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

solution2 = undefined
december06Solution2 :: IO Int
december06Solution2 = solution2 <$> input
