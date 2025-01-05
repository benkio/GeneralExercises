{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December21 where

import Data.Bifunctor (first)
import Data.Containers.ListUtils (nubOrd)
import Data.List (minimumBy)
import Data.Map (Map, adjust, elems, empty, fromList, insert, keys, (!?))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text, pack)
import Lib.Coord (Coord, coordPlus, manhattanDistanceSigned)
import Lib.CoordMove (coordMove, manhattanDistanceSignedToMove)
import Lib.List (filterByMostConsecutiveEqElems, prependToLists, rotate)
import Lib.Move (Move (..))
import Text.Printf (printf)

type NumericCode = [NumericKeypadBtn]
type DirectionalCode = [DirectionalKeypadBtn]
type DirectionalMemoryCount = Map (Int, Coord, DirectionalCode) Int
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
                robotMovess = if null rotatedMovess then [[PushA]] else (++ [PushA]) . fmap RM <$> rotatedMovess
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
                robotMovess = if null rotatedMovess then [[PushA]] else (++ [PushA]) . fmap RM <$> rotatedMovess
             in (,robotPos `coordPlus` distance) . nubOrd $ robotMovess
        )
        $ directionalKeypad !? targetBtn

rotateMovesKeepValid :: Coord -> [Move] -> Coord -> [[Move]]
rotateMovesKeepValid currentPos robotMoves invalidCoord =
    nubOrd $ mapMaybe tryRotate [0 .. (length robotMoves - 1)]
  where
    tryRotate rotation = if invalidCoord `elem` transitionedCoords (rotate rotation robotMoves) then Nothing else Just (rotate rotation robotMoves)
    transitionedCoords = scanl (flip coordMove) currentPos

movesSequenceNumericKeypad :: NumericCode -> [[RobotMove]]
movesSequenceNumericKeypad code = fst $ go initialNumericKeypadPosition code
  where
    go :: Coord -> NumericCode -> ([[RobotMove]], Coord)
    go robotPos [] = ([], robotPos)
    go robotPos (c : cs) = (\(mss, robotPos') -> first (prependToLists mss) (go robotPos' cs)) $ robotarmToNumericKeypad robotPos c

movesSequenceDirectionalKeypad' :: Coord -> DirectionalCode -> ([[RobotMove]], Coord)
movesSequenceDirectionalKeypad' robotPos [] = ([], robotPos)
movesSequenceDirectionalKeypad' robotPos (c : cs) =
    ( \(ms, robotPos') ->
        let (rs, c') = computeTail robotPos'
         in (ms ++ rs, c')
    )
        computeHead
  where
    computeHead = robotarmToDirectionalKeypad robotPos c
    computeTail robotPos' = movesSequenceDirectionalKeypad' robotPos' cs

robotMovesInitialMap :: Map (Coord, DirectionalCode) ([[RobotMove]], Coord)
robotMovesInitialMap = fromList $ (\(c, sdc) -> ((c, sdc), movesSequenceDirectionalKeypad' c sdc)) <$> singleDirectionalCodes
  where
    singleDirectionalCodes :: [(Coord, DirectionalCode)]
    singleDirectionalCodes = concatMap (\x -> fmap (,[x]) (elems directionalKeypad)) . keys $ directionalKeypad

robotMoveToDirectionalKeypadBtn :: RobotMove -> DirectionalKeypadBtn
robotMoveToDirectionalKeypadBtn PushA = DKPA
robotMoveToDirectionalKeypadBtn (RM m) = M m
robotMovesToDirectionalCode :: [RobotMove] -> DirectionalCode
robotMovesToDirectionalCode = fmap robotMoveToDirectionalKeypadBtn
numericRobotToDirectionalRobot :: NumericCode -> [DirectionalCode]
numericRobotToDirectionalRobot nc = robotMovesToDirectionalCode <$> movesSequenceNumericKeypad nc

evaluateSingleNext :: DirectionalMemoryCount -> Map Int Coord -> Int -> Coord -> DirectionalCode -> (Int, Map Int Coord, DirectionalMemoryCount)
evaluateSingleNext mem coordMap n currentC next =
    (result, coordMap', mem'')
  where
    (result, coordMap', mem') =
        maybe
            ( foldl
                ( \(acc, cm, m) nex ->
                    let (v, cm', m') = expandSingleDirectionalBtn m n nex cm
                     in (acc + v, cm', m')
                )
                (0, coordMap, mem)
                next
            )
            (, coordMap, mem)
            (mem !? (n, currentC, next))
    mem'' :: DirectionalMemoryCount
    mem'' = insert (n, currentC, next) result mem'

expandSingleDirectionalBtn :: DirectionalMemoryCount -> Int -> DirectionalKeypadBtn -> Map Int Coord -> (Int, Map Int Coord, DirectionalMemoryCount)
expandSingleDirectionalBtn mem 0 DKPA coordMap = (1, insert 0 initialDirectionalKeypadPosition coordMap, mem)
expandSingleDirectionalBtn mem 0 btn coordMap = (1, coordMap', mem)
  where
    coordMap' = adjust nextCurrentC 0 coordMap
    nextCurrentC c = snd . fromMaybe (error "The move should be present in the robotMovesMap") $ robotMovesInitialMap !? (c, [btn])
expandSingleDirectionalBtn mem n btn coordMap =
    ( result
    , coordMap''
    , mem'
    )
  where
    currentC = fromMaybe (error ("expected coord in coordMap n: " ++ show n)) $ coordMap !? n
    (nexts, _) = first (fmap robotMovesToDirectionalCode) . fromMaybe (error "The move should be present in the robotMovesMap") $ robotMovesInitialMap !? (currentC, [btn])
    nextCurrentC = fromMaybe (error "expected btn to exist in the directional pad") $ directionalKeypad !? btn
    (resultsCoordMap, mem') =
        foldl
            ( \(acc, m) nex ->
                let (v, cm', m') = evaluateSingleNext m coordMap (n - 1) currentC nex
                 in (acc ++ [(v, cm')], m')
            )
            ([], mem)
            nexts
    (result, coordMap') = minimumBy (comparing fst) resultsCoordMap
    coordMap'' = insert n nextCurrentC coordMap'
    upC' = fromMaybe (error ("expected coord in coordMap n: " ++ show n)) $ coordMap'' !? (n - 1)

-- <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
-- v<<A>>^A<A>AvA<^AA>A<vAAA>^A
-- <A^A>^^AvvvA
test =
    let dc = [M L, DKPA, M U, DKPA, M R, M U, M U, DKPA, M D, M D, M D, DKPA]
     in expandDirectionalCode empty 2 dc

expandDirectionalCode :: DirectionalMemoryCount -> Int -> DirectionalCode -> (Int, DirectionalMemoryCount)
expandDirectionalCode mem n code = (\(x, y, z) -> (x, z)) $ foldl computeDirectionalCode (0, coordMap, mem) code
  where
    computeDirectionalCode (acc, c, m) dbtn = (\(x, y, z) -> (acc + x, y, z)) $ expandSingleDirectionalBtn m n dbtn c
    coordMap = fromList $ (,initialDirectionalKeypadPosition) <$> [0 .. n]

shortestButtonSequence :: DirectionalMemoryCount -> Int -> NumericCode -> (Int, DirectionalMemoryCount)
shortestButtonSequence mem robotNum nc = (r, mem')
  where
    (r, mem') =
        first minimum
            $ foldl
                ( \(acc, m) dc ->
                    first ((acc ++) . (: [])) (expandDirectionalCode m robotNum dc)
                )
                ([], mem)
            $ numericRobotToDirectionalRobot nc

solution :: Int -> [NumericCode] -> Int
solution robotNum ncs = fst $ foldl go (0, empty) ncs
  where
    numericParts :: NumericCode -> Int
    numericParts = sum . zipWith (*) (iterate (* 10) 1) . reverse . mapMaybe getNum
    go (acc, mem) c =
        let (shortestLength, mem') = shortestButtonSequence mem robotNum c
         in (acc + shortestLength * numericParts c, mem')

december21Solution1 :: IO Int
december21Solution1 = solution 2 <$> input

-- -- too high 215929898128098
--             189235298434780
-- --          86261890651796
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
