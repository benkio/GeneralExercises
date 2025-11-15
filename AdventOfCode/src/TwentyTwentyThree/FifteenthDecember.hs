module TwentyTwentyThree.FifteenthDecember where

import Data.Bifunctor (bimap, first, second)
import Data.Char (ord)
import Data.List.Split (splitOn)
import Data.Map (Map, foldrWithKey)
import qualified Data.Map as M (alter, empty)

data LensesOp
    = LA {labelA :: String, focal :: Int}
    | LR {labelR :: String}
    deriving (Show)

data Lens = L {l :: String, v :: Int} deriving (Show)

type Box = [Lens]

type Boxes = Map Int Box

instance Read LensesOp where
    readsPrec _ s =
        let (label, op : rest) = break (`elem` "=-") s
            (mayFocal, rest') = second (drop 1) $ break (== ',') rest
         in if op == '-' then [(LR{labelR = label}, rest')] else [(LA{labelA = label, focal = read mayFocal :: Int}, rest')]

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

getLabel :: LensesOp -> String
getLabel (LR{labelR = label''}) = label''
getLabel (LA{labelA = label'', focal = f}) = label''

executeLensOp :: Boxes -> LensesOp -> Boxes
executeLensOp bs lop = M.alter (pure . maybe (opLens [] lop) (`opLens` lop)) opBoxNum bs
  where
    opBoxNum = (hashAlgorithmS . getLabel) lop

calculateFocusingPower :: Boxes -> Int
calculateFocusingPower = foldrWithKey calculateFocusingPowerBox 0
  where
    calculateFocusingPowerBox :: Int -> Box -> Int -> Int
    calculateFocusingPowerBox boxindex box acc =
        ((acc +) . sum) $ (\(i, l) -> product [1 + boxindex, i, v l]) <$> zip [1 ..] box

solution2 :: [LensesOp] -> Int
solution2 = calculateFocusingPower . foldl executeLensOp M.empty

fifteenthDecemberSolution2 :: IO Int
fifteenthDecemberSolution2 = solution2 <$> input'
