-------------------------------------------------------------------------------
--                           Advent Of Code - day 14                          --
-------------------------------------------------------------------------------
module TwentyTwenty.FourteenthDecember where

import Data.List (deleteBy, insertBy, isPrefixOf, stripPrefix)
import Data.Maybe (fromJust)

type Memory = [(Int, Int)]

data Instruction
  = Mask String
  | Mem
      { maddr :: Int
      , value :: Int
      }
  deriving (Show)

memory :: [Int] -> Memory
memory addresses = [(a, 0) | a <- addresses]

writeValueInMemory :: Int -> Int -> Memory -> Memory
writeValueInMemory v a m =
  insertBy orderingByAddress (a, v) $ deleteBy orderingByAddress' (a, v) m
  where
    orderingByAddress (a', _) (a'', _) = compare a' a''
    orderingByAddress' (a', _) (a'', _) = a' == a''

input :: IO [String]
input = lines <$> readFile "input/2020/14December.txt"

parseInput :: String -> Instruction
parseInput s
  | "mask = " `isPrefixOf` s = Mask $ (fromJust . stripPrefix "mask = ") s
  | "mem[" `isPrefixOf` s =
    Mem
      { maddr =
          ((\x -> read x :: Int) .
           takeWhile (']' /=) . fromJust . stripPrefix "mem[")
            s
      , value =
          ((\x -> read x :: Int) .
           tail . dropWhile ('=' /=) . fromJust . stripPrefix "mem[")
            s
      }
  | otherwise = error $ "unexpected value in input " ++ s

initialState :: (Memory, Instruction)
initialState = ([], Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

executeInstructions ::
     (Memory, Instruction) -> [Instruction] -> (Memory, Instruction)
executeInstructions (mem, mask) ((Mem {maddr = a, value = v}):is) =
  let v' = applyMask mask v
   in executeInstructions (writeValueInMemory v' a mem, mask) is
executeInstructions (mem, _) ((Mask mask'):is) =
  executeInstructions (mem, Mask mask') is
executeInstructions r [] = r

applyMask :: Instruction -> Int -> Int
applyMask (Mask m) v = (fromBinary 35 . applyMaskBinary m . toBinary 35) v
applyMask _ _ = error "Expected Mask instruction in input"

toBinary :: Int -> Int -> String
toBinary exp v
  | exp >= 0 =
    let (bit, rest) = v `divMod` (2 ^ exp)
     in show bit ++ toBinary (exp - 1) rest
  | otherwise = ""

fromBinary :: Int -> String -> Int
fromBinary _ [] = 0
fromBinary pos (bit:bits)
  | bit == '0' && pos >= 0 = fromBinary (pos - 1) bits
  | bit == '1' && pos >= 0 = (2 ^ pos) + fromBinary (pos - 1) bits
  | otherwise = 0

applyMaskBinary :: String -> String -> String
applyMaskBinary _ [] = []
applyMaskBinary [] x = x
applyMaskBinary (m:mask) (b:binaryNum)
  | m == 'X' = b : applyMaskBinary mask binaryNum
  | otherwise = m : applyMaskBinary mask binaryNum

fourteenthDecemberSolution1 :: IO Int
fourteenthDecemberSolution1 =
  sum . fmap snd . fst . executeInstructions initialState . fmap parseInput <$>
  input

testInput :: String
testInput =
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
            \mem[8] = 11\n\
            \mem[7] = 101\n\
            \mem[8] = 0"
