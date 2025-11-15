module TwentyTwentyTwo.TwentyFirstDecember where

import Data.Bifunctor (second)
import Data.Map (Map, adjust, fromList, partition, union)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust, maybe)
import Debug.Trace
import Text.Printf
import Text.Read (readMaybe)

root = "root"

hmn = "humn"

type MonkeyId = String

data MonkeyValue
    = MV Int
    | Add MonkeyId MonkeyId
    | Sub MonkeyId MonkeyId
    | Div MonkeyId MonkeyId
    | Mul MonkeyId MonkeyId
    deriving (Show)

data MonkeyValueResolved
    = MVR Int
    | HV
    | AddR MonkeyValueResolved MonkeyValueResolved
    | SubR MonkeyValueResolved MonkeyValueResolved
    | DivR MonkeyValueResolved MonkeyValueResolved
    | MulR MonkeyValueResolved MonkeyValueResolved
    deriving (Show, Eq)

data Monkey = Monkey
    { mid :: MonkeyId
    , value :: Either MonkeyValue MonkeyValueResolved
    }
    deriving (Show)

isResolved :: Monkey -> Maybe MonkeyValueResolved
isResolved (Monkey{value = v}) =
    either (const Nothing) Just v

resolveMonkeyOp :: Either MonkeyValue MonkeyValueResolved -> Map MonkeyId Monkey -> Either MonkeyValue MonkeyValueResolved
resolveMonkeyOp (Left mv) m = resolveMonkeyOp' mv m
resolveMonkeyOp x _ = x

resolveMonkeyOp' :: MonkeyValue -> Map MonkeyId Monkey -> Either MonkeyValue MonkeyValueResolved
resolveMonkeyOp' v@(MV x) _ = (Right . MVR) x
resolveMonkeyOp' v@(Add mi1 mi2) m = maybe (Left v) (\(v1, v2) -> Right (AddR v1 v2)) $ extractValues mi1 mi2 m
resolveMonkeyOp' v@(Sub mi1 mi2) m = maybe (Left v) (\(v1, v2) -> Right (SubR v1 v2)) $ extractValues mi1 mi2 m
resolveMonkeyOp' v@(Div mi1 mi2) m = maybe (Left v) (\(v1, v2) -> Right (DivR v1 v2)) $ extractValues mi1 mi2 m
resolveMonkeyOp' v@(Mul mi1 mi2) m = maybe (Left v) (\(v1, v2) -> Right (MulR v1 v2)) $ extractValues mi1 mi2 m

extractValues :: MonkeyId -> MonkeyId -> Map MonkeyId Monkey -> Maybe (MonkeyValueResolved, MonkeyValueResolved)
extractValues mi1 mi2 m = do
    mv1 <- isResolved =<< M.lookup mi1 m
    mv2 <- isResolved =<< M.lookup mi2 m
    return (mv1, mv2)

resolutionCycle :: Map MonkeyId Monkey -> Map MonkeyId Monkey
resolutionCycle m = resolved `union` newResolved
  where
    (resolved, unresolved) = partition (isJust . isResolved) m
    newResolved =
        M.map
            ( \mon ->
                mon{value = resolveMonkeyOp (value mon) resolved}
            )
            unresolved

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

resolveYelling :: Map MonkeyId Monkey -> MonkeyValueResolved
resolveYelling =
    fromJust
        . (=<<) isResolved
        . M.lookup root
        . until (all (isJust . isResolved)) resolutionCycle

interpret :: MonkeyValueResolved -> MonkeyValueResolved
interpret (AddR (MVR x) (MVR y)) = MVR (x + y)
interpret (SubR (MVR x) (MVR y)) = MVR (x - y)
interpret (DivR (MVR x) (MVR y)) = MVR (x `div` y)
interpret (MulR (MVR x) (MVR y)) = MVR (x * y)
interpret (AddR mv1 mv2) = AddR (interpret mv1) (interpret mv2)
interpret (SubR mv1 mv2) = SubR (interpret mv1) (interpret mv2)
interpret (DivR mv1 mv2) = DivR (interpret mv1) (interpret mv2)
interpret (MulR mv1 mv2) = MulR (interpret mv1) (interpret mv2)
interpret x = x

solution1 = fromJust . extractInt . until (isJust . extractInt) interpret . resolveYelling
  where
    extractInt (MVR x) = Just x
    extractInt x = Nothing

twentyFirstDecemberSolution1 :: IO Int
twentyFirstDecemberSolution1 = solution1 <$> input

solution2 m = (findHumanValue . simplify) yellingResolved
  where
    yellingResolved = (resolveYelling . adjust (\mon -> mon{value = Right HV}) hmn) m

simplify v = if v == v' then v else simplify v'
  where
    v' = interpret v

findHumanValue (AddR mv1 mv2) = findHumanValue' [(min b1 b2) .. (max b1 b2)]
  where
    y = (extractUnsafe . simplify) mv2
    f n = (extractUnsafe . simplify . assignHumanValue n) mv1
    steps = [100000000000, 1000000000, 10000000, 100000, 1000]
    jump s step = (\(x, y) -> (min x y, max x y)) $ until (\(a, b) -> max (f b) (f a) >= y && min (f a) (f b) <= y) (\(_, y) -> (y, y + step)) (s, s + step)
    (b1, b2) = foldl (\(x, _) s -> jump x s) (0, 0) steps
    extractUnsafe (MVR x) = x
    findHumanValue' [] = 0
    findHumanValue' (x : xs) = if f x == y then x else findHumanValue' xs

assignHumanValue :: Int -> MonkeyValueResolved -> MonkeyValueResolved
assignHumanValue v HV = MVR v
assignHumanValue _ (MVR x) = MVR x
assignHumanValue v (AddR mv1 mv2) = AddR (assignHumanValue v mv1) (assignHumanValue v mv2)
assignHumanValue v (SubR mv1 mv2) = SubR (assignHumanValue v mv1) (assignHumanValue v mv2)
assignHumanValue v (DivR mv1 mv2) = DivR (assignHumanValue v mv1) (assignHumanValue v mv2)
assignHumanValue v (MulR mv1 mv2) = MulR (assignHumanValue v mv1) (assignHumanValue v mv2)

twentyFirstDecemberSolution2 :: IO Int
twentyFirstDecemberSolution2 = solution2 <$> input

parseInput :: String -> Map MonkeyId Monkey
parseInput = fromList . fmap parseMonkey . lines

parseMonkey :: String -> (MonkeyId, Monkey)
parseMonkey = (\(i, v) -> (i, Monkey{mid = i, value = v})) . second (parseMonkeyVal . drop 2) . break (== ':')

parseMonkeyVal :: String -> Either MonkeyValue MonkeyValueResolved
parseMonkeyVal s = maybe (parseMonkeyValOp s) (Left . MV) mayNum
  where
    mayNum = readMaybe s :: Maybe Int

parseMonkeyValOp :: String -> Either MonkeyValue MonkeyValueResolved
parseMonkeyValOp s = case op of
    '+' -> Left $ Add (firstMonkey s) (secondMonkey s)
    '-' -> Left $ Sub (firstMonkey s) (secondMonkey s)
    '/' -> Left $ Div (firstMonkey s) (secondMonkey s)
    '*' -> Left $ Mul (firstMonkey s) (secondMonkey s)
  where
    firstMonkey = take 4
    op = s !! 5
    secondMonkey = take 4 . drop 7
