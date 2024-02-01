module TwentyTwentyThree.FifteenthDecember where

import Data.Map (Map)

import Data.Bifunctor (bimap, first, second)

import Data.Char (ord)
import Data.List.Split (splitOn)

data LensesOp
    = LA {labelA :: String, focal :: Int}
    | LR {labelR :: String}
    deriving (Show)
data Lens = L {l :: String, v :: Int}
type Box = [Lens]
type Boxes = Map Int Box

instance Read LensesOp where
    readsPrec _ s =
        let
            (label, (op : rest)) = break (`elem` "=-") s
            (mayFocal, rest') = second (drop 1) $ break (== ',') rest
         in
            if op == '-' then [(LR{labelR = label}, rest')] else [(LA{labelA = label, focal = read mayFocal :: Int}, rest')]

input :: IO [String]
input = parseInput . head . lines <$> readFile "input/2023/15December.txt"

input' :: IO [LensesOp]
input' = parseInputLenses . head . lines <$> readFile "input/2023/15December.txt"

parseInput :: String -> [String]
parseInput = splitOn ","

parseInputLenses :: String -> [LensesOp]
parseInputLenses = fmap (\x -> read x :: LensesOp) . parseInput

testInput :: [String]
testInput = parseInput "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

testInput' :: [LensesOp]
testInput' = parseInputLenses "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

hashAlgorithmC :: Int -> Char -> Int
hashAlgorithmC v = (`mod` 256) . (* 17) . (+ v) . ord

hashAlgorithmS :: String -> Int
hashAlgorithmS = foldl hashAlgorithmC 0

solution1 :: [String] -> Int
solution1 = sum . fmap hashAlgorithmS

-- 507007 too high
fifteenthDecemberSolution1 :: IO Int
fifteenthDecemberSolution1 = solution1 <$> input

opLens :: Box -> LensesOp -> Box
opLens (b : bs) (LR{labelR = label''}) = if l b == label'' then bs else b : opLens bs (LR{labelR = label''})
opLens (b : bs) (LA{labelA = label'', focal = f}) = if l b == label'' then L{l = label'', v = f} : bs else b : opLens bs (LA{labelA = label'', focal = f})
opLens [] (LR{}) = []
opLens [] (LA{labelA = label'', focal = f}) = [L{l = label'', v = f}]

executeLensOp :: Boxes -> LensesOp -> Boxes
executeLensOp = undefined

-- solution2 :: 
-- solution2 = foldl ()

fifteenthDecemberSolution2 :: IO Int
fifteenthDecemberSolution2 = undefined
