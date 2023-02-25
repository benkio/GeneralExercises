module TwentyTwentyTwo.TwentyFirstDecember where

import Data.Bifunctor (second)
import Data.Map (Map, fromList, partition, union)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, maybe)
import Text.Read (readMaybe)

type MonkeyId = String
data MonkeyValue
    = MV Int
    | Add MonkeyId MonkeyId
    | Sub MonkeyId MonkeyId
    | Div MonkeyId MonkeyId
    | Mul MonkeyId MonkeyId
    deriving (Show)
data Monkey = Monkey
    { mid :: MonkeyId
    , value :: MonkeyValue
    }
    deriving (Show)

isResolved :: MonkeyValue -> Maybe Int
isResolved (MV i) = Just i
isResolved _ = Nothing

resolveMonkeyOp :: MonkeyValue -> Map MonkeyId Monkey -> MonkeyValue
resolveMonkeyOp v@(MV _) _ = v
resolveMonkeyOp v@(Add mi1 mi2) m = maybe v (\(v1, v2) -> MV (v1 + v2)) $ extractValues mi1 mi2 m
resolveMonkeyOp v@(Sub mi1 mi2) m = maybe v (\(v1, v2) -> MV (v1 - v2)) $ extractValues mi1 mi2 m
resolveMonkeyOp v@(Div mi1 mi2) m = maybe v (\(v1, v2) -> MV (v1 `div` v2)) $ extractValues mi1 mi2 m
resolveMonkeyOp v@(Mul mi1 mi2) m = maybe v (\(v1, v2) -> MV (v1 * v2)) $ extractValues mi1 mi2 m

extractValues :: MonkeyId -> MonkeyId -> Map MonkeyId Monkey -> Maybe (Int, Int)
extractValues mi1 mi2 m = do
    mv1 <- (isResolved . value) =<< M.lookup mi1 m
    mv2 <- (isResolved . value) =<< M.lookup mi2 m
    return (mv1, mv2)

resolutionCycle :: Map MonkeyId Monkey -> Map MonkeyId Monkey
resolutionCycle m = union resolved newResolved
  where
    (resolved, unresolved) = partition (isJust . isResolved . value) m
    newResolved = M.map (\mon -> mon{value = resolveMonkeyOp (value mon) (resolved)}) unresolved

input :: IO (Map MonkeyId Monkey)
input = parseInput <$> readFile "input/2022/21December.txt"

testInput :: Map MonkeyId Monkey
testInput =
    parseInput
        "root: pppw + sjmn\n\
        \dbpl: 5\n\
        \cczh: sllz + lgvd\n\
        \zczc: 2\n\
        \ptdq: humn - dvpt\n\
        \dvpt: 3\n\
        \lfqf: 4\n\
        \humn: 5\n\
        \ljgn: 2\n\
        \sjmn: drzm * dbpl\n\
        \sllz: 4\n\
        \pppw: cczh / lfqf\n\
        \lgvd: ljgn * ptdq\n\
        \drzm: hmdt - zczc\n\
        \hmdt: 32"

solution1 :: Map MonkeyId Monkey -> Int
solution1 =
    fromJust
        . ((=<<) (isResolved . value))
        . M.lookup "root"
        . until (all (isJust . isResolved . value)) resolutionCycle

twentyFirstDecemberSolution1 :: IO Int
twentyFirstDecemberSolution1 = solution1 <$> input

twentyFirstDecemberSolution2 :: IO Int
twentyFirstDecemberSolution2 = undefined

parseInput :: String -> Map MonkeyId Monkey
parseInput = fromList . fmap parseMonkey . lines

parseMonkey :: String -> (MonkeyId, Monkey)
parseMonkey = ((\(i, v) -> (i, Monkey{mid = i, value = v})) . second (parseMonkeyVal . drop 2) . break (== ':'))

parseMonkeyVal :: String -> MonkeyValue
parseMonkeyVal s = maybe (parseMonkeyValOp s) (MV) mayNum
  where
    mayNum = readMaybe s :: Maybe Int

parseMonkeyValOp :: String -> MonkeyValue
parseMonkeyValOp s = case op of
    '+' -> Add (firstMonkey s) (secondMonkey s)
    '-' -> Sub (firstMonkey s) (secondMonkey s)
    '/' -> Div (firstMonkey s) (secondMonkey s)
    '*' -> Mul (firstMonkey s) (secondMonkey s)
  where
    firstMonkey = take 4
    op = s !! 5
    secondMonkey = take 4 . drop 7
