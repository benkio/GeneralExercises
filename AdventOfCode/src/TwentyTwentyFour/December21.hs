{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December21 where

import Data.Ord (comparing)

import Control.Arrow ((>>>))
import Control.Monad ((>=>))
import Data.Bifunctor (bimap, first)
import Data.Containers.ListUtils (nubOrd)
import Data.List (minimumBy)
import Data.Map (Map, adjust, elems, empty, fromList, insert, keys, size, toList, (!?))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text, pack)
import Debug.Trace
import Lib.Coord (Coord, coordPlus, manhattanDistanceSigned)
import Lib.CoordMove (coordMove, manhattanDistanceSignedToMove)
import Lib.List (filterByMostConsecutiveEqElems, prependToLists, rotate)
import Lib.Move (Move (..))
import Text.Printf (printf)

type NumericCode = [NumericKeypadBtn]
type DirectionalCode = [DirectionalKeypadBtn]
type DirectionalMemory = Map (Coord, DirectionalCode) ([RobotMove], Coord)
type DirectionalMemoryCount = Map (Int, Coord, Coord, DirectionalCode) (Int, Coord)
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
robotarmToDirectionalKeypad :: Coord -> DirectionalKeypadBtn -> ([RobotMove], Coord)
robotarmToDirectionalKeypad robotPos targetBtn =
    maybe
        (error "[robotarmToDirectionalKeypad] Impossible WrongKeypad")
        ( \kpc ->
            let distance = manhattanDistanceSigned kpc robotPos
                moves = manhattanDistanceSignedToMove distance
                selectedMove = listToMaybe . filterByMostConsecutiveEqElems $ rotateMovesKeepValid robotPos moves invalidDirectionalKeypadCoord
                robotMoves = fromMaybe [PushA] $ (++ [PushA]) . fmap RM <$> selectedMove
             in (robotMoves, robotPos `coordPlus` distance)
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

movesSequenceDirectionalKeypad' :: DirectionalMemory -> Coord -> DirectionalCode -> ([RobotMove], Coord, DirectionalMemory)
movesSequenceDirectionalKeypad' mem robotPos [] = ([], robotPos, mem)
movesSequenceDirectionalKeypad' mem robotPos (c : cs) =
    ( \(ms, robotPos') ->
        let (rs, c', mem') = computeTail robotPos'
         in -- mem'' = insert (robotPos, chunk) computeHead mem'
            (ms ++ rs, c', mem')
    )
        $ computeHead
  where
    -- (chunk, rest) = (\(x, y) -> (x ++ [head y], tail y)) . break (== DKPA) $ code
    computeHead = robotarmToDirectionalKeypad robotPos c -- fromMaybe (foldl (\(acc, coord) c -> first (acc ++) (robotarmToDirectionalKeypad coord c)) ([], robotPos) chunk) $ mem !? (robotPos, chunk)
    computeTail robotPos' = movesSequenceDirectionalKeypad' mem robotPos' cs -- maybe (movesSequenceDirectionalKeypad' mem robotPos' cs) (\(x,y) -> (x,y,mem)) $ mem !? (robotPos', cs)

movesSequenceDirectionalKeypad :: DirectionalMemory -> DirectionalCode -> ([RobotMove], DirectionalMemory)
movesSequenceDirectionalKeypad mem code = (\(x, _, y) -> (x, y)) $ movesSequenceDirectionalKeypad' mem initialDirectionalKeypadPosition code

robotMoveToDirectionalKeypadBtn :: RobotMove -> DirectionalKeypadBtn
robotMoveToDirectionalKeypadBtn PushA = DKPA
robotMoveToDirectionalKeypadBtn (RM m) = M m
robotMovesToDirectionalCode :: [RobotMove] -> DirectionalCode
robotMovesToDirectionalCode = fmap robotMoveToDirectionalKeypadBtn

numericRobotToDirectionalRobot :: NumericCode -> [DirectionalCode]
numericRobotToDirectionalRobot nc = robotMovesToDirectionalCode <$> movesSequenceNumericKeypad nc
directionalRobotToDirectionalRobot :: (DirectionalCode, DirectionalMemory) -> (DirectionalCode, DirectionalMemory)
directionalRobotToDirectionalRobot (dc, mem) = (\(xs, mem') -> (robotMovesToDirectionalCode xs, mem')) $ movesSequenceDirectionalKeypad mem dc
yourMoves :: DirectionalMemory -> Int -> NumericCode -> ([RobotMove], DirectionalMemory)
yourMoves mem robotNum nc =
    first (minimumBy (comparing length))
        . ( \(m, rss) ->
                foldl (\(acc, m') rs -> first ((acc ++) . (: [])) (movesSequenceDirectionalKeypad m' rs)) ([], m) rss
          )
        <$> foldl go (mem, [])
        $ numericRobotToDirectionalRobot nc
  where
    go (m, acc) ds =
        let (rs, m') = pipeline (ds, m)
         in (m', acc ++ [rs])
    pipeline = foldl (>>>) directionalRobotToDirectionalRobot $ replicate (robotNum - 2) directionalRobotToDirectionalRobot

-- test =
--     "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A" `elem` (concat . fmap show <$> yourMoves 2 [Num 0, Num 2, Num 9, NKPA])

-- "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A" `elem` (concat . fmap show <$> yourMoves [Num 9,Num 8,Num 0,NKPA])
-- "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" `elem` (concat . fmap show <$> yourMoves [Num 1,Num 7,Num 9,NKPA])
-- "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A" `elem` (concat . fmap show <$> yourMoves [Num 4,Num 5,Num 6,NKPA])
-- "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A" `elem` (concat . fmap show <$> yourMoves 2 [Num 3, Num 7, Num 9, NKPA])

-- -- (movesSequenceDirectionalKeypad . robotMovesToDirectionalCode)
-- -- [RM D,RM L, RM L,PushA,RM R,RM R,RM U,PushA,RM L,PushA,RM R,PushA,RM D,PushA,RM L,RM U,PushA,PushA,RM R,PushA,RM L,RM D,PushA,PushA,PushA,RM R,RM U,PushA]

robotMovesInitialMap :: Map (Coord, DirectionalCode) ([RobotMove], Coord)
robotMovesInitialMap = fromList $ (\(c, sdc) -> ((c, sdc), (\(x, y, _) -> (x, y)) (movesSequenceDirectionalKeypad' empty c sdc))) <$> singleDirectionalCodes
  where
    singleDirectionalCodes :: [(Coord, DirectionalCode)]
    singleDirectionalCodes = concatMap (\x -> fmap (,[x]) (elems directionalKeypad)) . keys $ directionalKeypad

expandSingleDirectionalBtn :: DirectionalMemoryCount -> Int -> DirectionalKeypadBtn -> Map Int Coord -> (Int, Map Int Coord, DirectionalMemoryCount)
expandSingleDirectionalBtn mem 0 btn coordMap = trace "check" (1, adjust (nextCoord btn) 0 coordMap, mem)
expandSingleDirectionalBtn mem n btn coordMap =
    trace
        (printf ("next: " ++ show next ++ " - " ++ show n ++ " - " ++ show result ++ " - " ++ show (size mem)))
        ( result
        , coordMap''
        , mem''
        )
  where
    currentC = fromMaybe (error ("expected coord in coordMap n: " ++ show n)) $ coordMap !? n
    upC = fromMaybe (error ("expected coord in coordMap n: " ++ show n)) $ coordMap !? (n - 1)
    nextCurrentC = nextCoord btn currentC
    next = fst . first robotMovesToDirectionalCode . fromMaybe (error "there should be a sequence here") $ robotMovesInitialMap !? (upC, [btn])
    (result, coordMap', mem') =
        fromMaybe
            ( foldl
                ( \(acc, cm, m) nex ->
                    let
                        (v, cm', m') = expandSingleDirectionalBtn m (n - 1) nex cm
                     in
                        (acc + v, cm', m')
                )
                (0, coordMap, mem)
                next
            )
            $ (\(r, c) -> (r, insert (n - 1) c coordMap, mem)) <$> mem !? (n - 1, currentC, upC, next)
    coordMap'' = insert n nextCurrentC coordMap'
    upC' = fromMaybe (error ("expected coord in coordMap n: " ++ show n)) $ coordMap'' !? (n - 1)
    mem'' :: DirectionalMemoryCount
    mem'' = insert (n - 1, currentC, upC, next) (result, upC') mem'

expandDirectionalCode :: DirectionalMemoryCount -> Int -> DirectionalCode -> (Int, DirectionalMemoryCount)
expandDirectionalCode mem n code = (\(x, y, z) -> (x, z)) $ foldl computeDirectionalCode (0, coordMap, mem) code
  where
    computeDirectionalCode (acc, c, m) dbtn = (\(x, y, z) -> (acc + x, y, z)) $ expandSingleDirectionalBtn m n dbtn c
    coordMap = fromList $ (,initialDirectionalKeypadPosition) <$> [0 .. n]

nextCoord :: DirectionalKeypadBtn -> Coord -> Coord
nextCoord (M m) c = coordMove m c
nextCoord DKPA c = c

shortestButtonSequence :: DirectionalMemoryCount -> Int -> NumericCode -> (Int, DirectionalMemoryCount)
shortestButtonSequence mem robotNum nc = (r,  mem')
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

-- too high 215929898128098
--          86261890651796
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
