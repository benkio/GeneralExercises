{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December21 where

import Data.Ord (comparing)

import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.List (minimumBy)
import Data.Map (Map, elems, fromList, toList, (!?))
import Data.Maybe (mapMaybe)
import Data.Text (Text, pack)
import Debug.Trace
import Lib.Coord (Coord, coordPlus, manhattanDistanceSigned)
import Lib.CoordMove (coordMove, manhattanDistanceSignedToMove)
import Lib.List (prependToLists, rotate)
import Lib.Move (Move (..))
import Text.Printf (printf)
import Control.Monad ((>=>))

type NumericCode = [NumericKeypadBtn]
type DirectionalCode = [DirectionalKeypadBtn]
data NumericKeypadBtn = NKPA | Num Int deriving (Show, Eq, Ord)
data DirectionalKeypadBtn = DKPA | M Move deriving (Show, Eq, Ord)
data RobotMove = PushA | RM Move deriving (Eq, Ord)

instance Show RobotMove where
    show PushA = "A"
    show (RM move) = show move

input :: IO [NumericCode]
input = parseInput <$> readFile "input/2024/December21.txt"

getNum :: NumericKeypadBtn -> Maybe Int
getNum (Num x) = Just x
getNum _ = Nothing

robotarmToNumericKeypad :: Coord -> NumericKeypadBtn -> ([[RobotMove]], Coord)
robotarmToNumericKeypad robotPos targetBtn =
    maybe
        (error "[robotarmToNumericKeypad] Impossible WrongKeypad")
        ( \kpc ->
            let distance = manhattanDistanceSigned kpc robotPos
                moves = manhattanDistanceSignedToMove distance
                rotatedMovess = rotateMovesKeepValid robotPos moves invalidNumericKeypadCoord
                robotMovess = if null rotatedMovess then [[PushA]] else (++ [PushA]) . fmap (RM) <$> rotatedMovess
             in (,robotPos `coordPlus` distance) . nubOrd $ robotMovess
        )
        $ numericKeypad !? targetBtn
robotarmToDirectionalKeypad :: Coord -> DirectionalKeypadBtn -> ([[RobotMove]], Coord)
robotarmToDirectionalKeypad robotPos targetBtn =
    maybe
        (error "[robotarmToDirectionalKeypad] Impossible WrongKeypad")
        ( \kpc ->
            let distance = manhattanDistanceSigned kpc robotPos
                moves = manhattanDistanceSignedToMove distance
                rotatedMovess = rotateMovesKeepValid robotPos moves invalidDirectionalKeypadCoord
                robotMovess = if null rotatedMovess then [[PushA]] else (++ [PushA]) . fmap (RM) <$> rotatedMovess
             in (,robotPos `coordPlus` distance) . nubOrd $ robotMovess
        )
        $ directionalKeypad !? targetBtn

rotateMovesKeepValid :: Coord -> [Move] -> Coord -> [[Move]]
rotateMovesKeepValid currentPos robotMoves invalidCoord =
    mapMaybe tryRotate [0 .. (length robotMoves - 1)]
  where
    tryRotate rotation = if invalidCoord `elem` transitionedCoords (rotate rotation robotMoves) then Nothing else Just (rotate rotation robotMoves)
    transitionedCoords = scanl (flip coordMove) currentPos

movesSequenceNumericKeypad :: NumericCode -> [[RobotMove]]
movesSequenceNumericKeypad code = fst $ go initialNumericKeypadPosition code
  where
    go :: Coord -> NumericCode -> ([[RobotMove]], Coord)
    go robotPos [] = ([], robotPos)
    go robotPos (c : cs) = (\(mss, robotPos') -> first (prependToLists mss) (go robotPos' cs)) $ robotarmToNumericKeypad robotPos c

movesSequenceDirectionalKeypad :: DirectionalCode -> [[RobotMove]]
movesSequenceDirectionalKeypad code = fst $ go initialDirectionalKeypadPosition code
  where
    go :: Coord -> DirectionalCode -> ([[RobotMove]], Coord)
    go robotPos [] = ([], robotPos)
    go robotPos (c : cs) = (\(mss, robotPos') -> first (prependToLists mss) (go robotPos' cs)) $ robotarmToDirectionalKeypad robotPos c

robotMoveToDirectionalKeypadBtn :: RobotMove -> DirectionalKeypadBtn
robotMoveToDirectionalKeypadBtn PushA = DKPA
robotMoveToDirectionalKeypadBtn (RM m) = M m
robotMovesToDirectionalCode :: [RobotMove] -> DirectionalCode
robotMovesToDirectionalCode = fmap robotMoveToDirectionalKeypadBtn

numericRobotToDirectionalRobot :: NumericCode -> [DirectionalCode]
numericRobotToDirectionalRobot nc = robotMovesToDirectionalCode <$> movesSequenceNumericKeypad nc
directionalRobotToDirectionalRobot :: DirectionalCode -> [DirectionalCode]
directionalRobotToDirectionalRobot dc = robotMovesToDirectionalCode <$> movesSequenceDirectionalKeypad dc
yourMoves :: Int -> NumericCode -> [[RobotMove]]
yourMoves robotNum nc = do
  secondDirectionalCode <- numericRobotToDirectionalRobot nc
  finalDirectionalCode <- (\f -> f secondDirectionalCode) $ foldl (>=>) directionalRobotToDirectionalRobot $ replicate (robotNum - 2) directionalRobotToDirectionalRobot
  movesSequenceDirectionalKeypad finalDirectionalCode
test =
    -- "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" `elem` (concat . fmap show <$> yourMoves [Num 0,Num 2,Num 9,NKPA])
    -- "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A" `elem` (concat . fmap show <$> yourMoves [Num 9,Num 8,Num 0,NKPA])
    -- "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" `elem` (concat . fmap show <$> yourMoves [Num 1,Num 7,Num 9,NKPA])
    -- "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A" `elem` (concat . fmap show <$> yourMoves [Num 4,Num 5,Num 6,NKPA])
    "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" `elem` (concat . fmap show  <$> yourMoves 2 [Num 3, Num 7, Num 9, NKPA])

-- (movesSequenceDirectionalKeypad . robotMovesToDirectionalCode)
-- [RM D,RM L, RM L,PushA,RM R,RM R,RM U,PushA,RM L,PushA,RM R,PushA,RM D,PushA,RM L,RM U,PushA,PushA,RM R,PushA,RM L,RM D,PushA,PushA,PushA,RM R,RM U,PushA]

shortestButtonSequence :: Int -> NumericCode -> Int
shortestButtonSequence robotNum = length . minimumBy (comparing length) . yourMoves robotNum

solution :: Int -> [NumericCode] -> Int
solution robotNum = foldl (\acc c -> acc + shortestButtonSequence robotNum c * numericParts c) 0
  where
    numericParts :: NumericCode -> Int
    numericParts = sum . zipWith (*) (iterate (* 10) 1) . reverse . mapMaybe getNum

december21Solution1 :: IO Int
december21Solution1 = solution 2 <$> input

december21Solution2 :: IO Int
december21Solution2 = solution 25 <$> input

parseInput :: String -> [NumericCode]
parseInput = (fmap . fmap) parseNumericKeypad . lines
  where
    parseNumericKeypad 'A' = NKPA
    parseNumericKeypad '1' = Num 1
    parseNumericKeypad '2' = Num 2
    parseNumericKeypad '3' = Num 3
    parseNumericKeypad '4' = Num 4
    parseNumericKeypad '5' = Num 5
    parseNumericKeypad '6' = Num 6
    parseNumericKeypad '7' = Num 7
    parseNumericKeypad '8' = Num 8
    parseNumericKeypad '9' = Num 9
    parseNumericKeypad '0' = Num 0

testInput :: [NumericCode]
testInput =
    parseInput
        "029A\n\
        \980A\n\
        \179A\n\
        \456A\n\
        \379A\n"

{-
+---+---+---+
\| 7 | 8 | 9 |
+---+---+---+
\| 4 | 5 | 6 |
+---+---+---+
\| 1 | 2 | 3 |
+---+---+---+
    | 0 | A |
    +---+---+
-}
numericKeypad :: Map NumericKeypadBtn Coord
numericKeypad =
    fromList
        [ (Num 7, (0, 0))
        , (Num 8, (1, 0))
        , (Num 9, (2, 0))
        , (Num 4, (0, 1))
        , (Num 5, (1, 1))
        , (Num 6, (2, 1))
        , (Num 1, (0, 2))
        , (Num 2, (1, 2))
        , (Num 3, (2, 2))
        , (Num 0, (1, 3))
        , (NKPA, (2, 3))
        ]

{-
    +---+---+
    | ^ | A |
+---+---+---+
\| < | v | > |
+---+---+---+
-}
directionalKeypad :: Map DirectionalKeypadBtn Coord
directionalKeypad =
    fromList
        [ (M U, (1, 0))
        , (DKPA, (2, 0))
        , (M L, (0, 1))
        , (M D, (1, 1))
        , (M R, (2, 1))
        ]

initialNumericKeypadPosition, initialDirectionalKeypadPosition :: Coord
initialNumericKeypadPosition = (2, 3)
initialDirectionalKeypadPosition = (2, 0)

invalidNumericKeypadCoord, invalidDirectionalKeypadCoord :: Coord
invalidNumericKeypadCoord = (0, 3)
invalidDirectionalKeypadCoord = (0, 0)
