module TwentyTwentyTwo.TwentiethDecember where

import Data.Maybe (fromJust)
import Data.Sequence (Seq, cycleTaking, deleteAt, elemIndexL, foldlWithIndex, fromList, index, insertAt, spanl, (><))
import qualified Data.Sequence as S (length)
import Debug.Trace

data EncryptedValue = EncryptedValue {eid :: Int, value :: Int} deriving (Eq)

instance Show EncryptedValue where
    show = show . value

parseInput = fromList . fmap (\(i, x) -> EncryptedValue{eid = i, value = (read x :: Int)}) . ([0 ..] `zip`) . lines

input :: IO (Seq EncryptedValue)
input = parseInput <$> readFile "input/2022/20December.txt"

testInput :: Seq EncryptedValue
testInput =
    parseInput
        "1\n\
        \2\n\
        \-3\n\
        \3\n\
        \-2\n\
        \0\n\
        \4"

calculateEndPosition :: EncryptedValue -> Seq EncryptedValue -> Int
calculateEndPosition e es
    | movement == 0 = epos
    | endIndex == 0 = esLength
    | endIndex == esLength = 0
    | otherwise = mod endIndex esLength
  where
    epos = fromJust $ elemIndexL e es
    esLength = S.length es - 1
    movement = signum (value e) * (mod (abs (value e)) esLength)
    endIndex = epos + movement

moveElem :: EncryptedValue -> Int -> Seq EncryptedValue -> Seq EncryptedValue
moveElem e endIndex es = if endIndex == startIndex then es else es'
  where
    startIndex = fromJust $ elemIndexL e es
    es' = insertAt endIndex e $ deleteAt startIndex es

moveElems :: Seq EncryptedValue -> Seq EncryptedValue
moveElems es = foldlWithIndex (\acc _ e -> trace ("acc: " ++ show acc ++ " elem: " ++ show e) (moveElem e (calculateEndPosition e acc) acc)) es es

findCoordinates :: Seq EncryptedValue -> [Int]
findCoordinates es = [coordinate 1000, coordinate 2000, coordinate 3000]
  where
    (rest, zeroHead) = spanl ((/= 0) . value) es
    longEs = cycleTaking 3001 (zeroHead >< rest)
    coordinate = value . index longEs

test1 = moveElem zero (calculateEndPosition zero testInput) testInput == testInput
  where
    zero = EncryptedValue{eid = 5, value = 0}
test2 = solution testInput == 3
test3 = (findCoordinates . moveElems) testInput == [4, -3, 2]
test4 = (moveElems . parseInput) "9\n0\n1\n-5"
test = test1 && test2 && test3

solution = sum . findCoordinates . moveElems

-- 14118
-- 3139 2740 8239
-- twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = findCoordinates . moveElems <$> input

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = undefined
