module TwentyTwentyTwo.ThirteenthDecember where

import Data.List (sortBy)
import Data.List.Split (onSublist, split)
import GHC.Read (list)
import GHC.Show (appPrec)
import Text.Read

data Packet = PK Int | PKL [Packet] deriving (Show, Eq)

instance Read Packet where
    readPrec =
        parens
            ( do
                l <- list readPrec
                return (PKL l)
                +++ prec
                    appPrec
                    ( do
                        x <- step readPrec
                        return (PK x)
                    )
            )

input :: IO [(Int, Packet, Packet)]
input = parsePacket <$> readFile "input/2022/13December.txt"

parsePacket :: String -> [(Int, Packet, Packet)]
parsePacket =
    fmap (\((x : y : []), i) -> (i, read x :: Packet, read y :: Packet))
        . (`zip` [1 ..])
        . filter ((== 2) . length)
        . split (onSublist [""])
        . lines

testInput :: String
testInput =
    "[1,1,3,1,1]\n\
    \[1,1,5,1,1]\n\
    \\n\
    \[[1],[2,3,4]]\n\
    \[[1],4]\n\
    \\n\
    \[9]\n\
    \[[8,7,6]]\n\
    \\n\
    \[[4,4],4,4]\n\
    \[[4,4],4,4,4]\n\
    \\n\
    \[7,7,7,7]\n\
    \[7,7,7]\n\
    \\n\
    \[]\n\
    \[3]\n\
    \\n\
    \[[[]]]\n\
    \[[]]\n\
    \\n\
    \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
    \[1,[2,[3,[4,[5,6,0]]]],8,9]"

checkOrder :: Packet -> Packet -> Ordering
checkOrder (PK x) (PK y) = x `compare` y
checkOrder (PK x) (PKL ys) = checkOrderLists [(PK x)] ys
checkOrder (PKL xs) (PK y) = checkOrderLists xs [(PK y)]
checkOrder (PKL xs) (PKL ys) = checkOrderLists xs ys

checkOrderLists :: [Packet] -> [Packet] -> Ordering
checkOrderLists xs ys = if zipCompare == EQ then (length xs `compare` length ys) else zipCompare
  where
    zipCompare = mconcat $ uncurry checkOrder <$> xs `zip` ys

countOrderedPackets :: [(Int, Packet, Packet)] -> Int
countOrderedPackets = sum . fmap (\(x, _, _) -> x) . filter (\(_, p, p') -> (p `checkOrder` p') == LT)

thirteenthDecemberSolution1 :: IO Int
thirteenthDecemberSolution1 = countOrderedPackets <$> input

newInput1 = read "[[2]]" :: Packet
newInput2 = read "[[6]]" :: Packet

solution2Input :: [(Int, Packet, Packet)] -> [Packet]
solution2Input = (++ [newInput1, newInput2]) . concatMap (\(_, p, p') -> [p, p'])

solution2 :: [(Int, Packet, Packet)] -> Int
solution2 = product . fmap snd . filter (\(x, _) -> x == newInput1 || x == newInput2) . (`zip` [1 ..]) . sortBy checkOrder . solution2Input

thirteenthDecemberSolution2 :: IO Int
thirteenthDecemberSolution2 = solution2 <$> input
