module TwentySixteen.December02 where

import Data.List
import Data.Maybe

type Coordinate = (Int, Int)

data Instruction
    = U
    | D
    | L
    | R
    deriving (Read, Show)

input :: IO [[Instruction]]
input = fmap parseInstructions . lines <$> readFile "input/2016/2December.txt"

parseInstructions :: String -> [Instruction]
parseInstructions = fmap (\x -> read [x])

keypad :: [Coordinate]
keypad = [(x, y) | x <- [-1 .. 1], y <- [-1 .. 1]]

keypadMap :: [(Coordinate, Int)]
keypadMap = keypad `zip` [7, 4, 1, 8, 5, 2, 9, 6, 3]

applyInstruction :: [Coordinate] -> Coordinate -> Instruction -> Coordinate
applyInstruction ks c i =
    let nextCoordinate = applyInstruction' c i
     in if nextCoordinate `elem` ks
            then nextCoordinate
            else c

applyInstruction' :: Coordinate -> Instruction -> Coordinate
applyInstruction' (x, y) U = (x, y + 1)
applyInstruction' (x, y) D = (x, y - 1)
applyInstruction' (x, y) L = (x - 1, y)
applyInstruction' (x, y) R = (x + 1, y)

coordinateToKey :: Coordinate -> [(Coordinate, a)] -> a
coordinateToKey c = snd . fromJust . find ((c ==) . fst)

testInput :: [[Instruction]]
testInput =
    (fmap parseInstructions . lines)
        "ULL\n\
        \RRDDD\n\
        \LURDL\n\
        \UUUUD"

solution1Test :: Bool
solution1Test = solution (0, 0) keypadMap testInput == [1, 9, 8, 5]

solution :: Coordinate -> [(Coordinate, a)] -> [[Instruction]] -> [a]
solution _ _ [] = []
solution c m (i : is) =
    let c' = foldl (applyInstruction (fmap fst m)) c i
     in coordinateToKey c' m : solution c' m is

december02Solution1 :: IO String
december02Solution1 = concatMap show . solution (0, 0) keypadMap <$> input

newKeypad :: [Coordinate]
newKeypad = keypad ++ [(0, 2), (2, 0), (0, -2), (-2, 0)]

newKeypadMap :: [(Coordinate, String)]
newKeypadMap =
    newKeypad
        `zip` ["A", "6", "2", "B", "7", "3", "C", "8", "4", "1", "9", "D", "5"]

solution2Test :: Bool
solution2Test = concat (solution (-2, 0) newKeypadMap testInput) == "5DB3"

december02Solution2 :: IO String
december02Solution2 = concat . solution (-2, 0) newKeypadMap <$> input
