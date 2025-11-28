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

applyInstructions :: [LightBlock] -> [Instruction] -> [Light]
applyInstructions [] (("turn on", sc, ec) : is) = applyInstructions [(sc, ec)] is
applyInstructions [] (("toggle", sc, ec) : is) = applyInstructions [(sc, ec)] is
applyInstructions [] (_ : is) = applyInstructions [] is
applyInstructions ls [] = ls

-- Handle block collision and splitting, keeping only the light blocks
-- applyInstructions ls (("turn on", sc, ec) : is) =
--     let
--         newTurnedLights = diffMap fst (lights 1 sc ec) ls
--         newLights = ls ++ newTurnedLights
--      in
--         applyInstructions newLights is
-- applyInstructions ls (("turn off", sc, ec) : is) =
--     let
--         (_, stillOn) = span (\(c, _) -> inside c sc ec) ls
--      in
--         applyInstructions stillOn is
-- applyInstructions ls (("toggle", sc, ec) : is) =
--     let
--         (toggled, stillOn) = span (\(c, _) -> inside c sc ec) ls
--         newTurnedLights = diffMap fst (lights 1 sc ec) ls
--         newLights = stillOn ++ (filter ((== (-1)) . snd) . fmap (\(c, v) -> (c, v * (-1)))) toggled ++ newTurnedLights
--      in
--         applyInstructions newLights is

solution1 :: [Instruction] -> Int
solution1 is = undefined

december06Solution1 :: IO Int
december06Solution1 = solution1 <$> input

solution2 = undefined
december06Solution2 :: IO Int
december06Solution2 = solution2 <$> input
