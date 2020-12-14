-------------------------------------------------------------------------------
--                           Advent Of Code - day 14                          --
-------------------------------------------------------------------------------
module TwentyTwenty.FourteenthDecember where

import           Data.List  (isPrefixOf, stripPrefix)
import           Data.Map   (Map, elems, empty, insert)
import           Data.Maybe (fromJust)

type Memory = Map Int Int

data Instruction
  = Mask String
  | Mem
      { maddr :: Int
      , value :: Int
      }
  deriving (Show)

writeValueInMemory :: Int -> Int -> Memory -> Memory
writeValueInMemory v a m = insert a v m

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
initialState = (empty, Mask "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")

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
applyMask _ _        = error "Expected Mask instruction in input"

toBinary :: Int -> Int -> String
toBinary exp' v
  | exp' >= 0 =
    let (bit, rest) = v `divMod` (2 ^ exp')
     in show bit ++ toBinary (exp' - 1) rest
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
  sum . elems . fst . executeInstructions initialState . fmap parseInput <$>
  input

executeInstructionsV2 ::
     (Memory, Instruction) -> [Instruction] -> (Memory, Instruction)
executeInstructionsV2 (mem, mask) (Mem {maddr = a, value = v}:is) =
  let addresses = applyMaskFloating mask a
      newState =
        foldl
          (\(m, msk') addr -> (writeValueInMemory v addr m, msk'))
          (mem, mask)
          addresses
   in executeInstructionsV2 newState is
executeInstructionsV2 (mem, _) ((Mask mask'):is) =
  executeInstructionsV2 (mem, Mask mask') is
executeInstructionsV2 r [] = r

applyMaskFloating :: Instruction -> Int -> [Int]
applyMaskFloating (Mask mask) a =
  let binaryAddress = toBinary 35 a
      floatingAddress = applyMaskBinaryFloating mask binaryAddress
   in fromBinary 35 <$> unfoldFloatingAddress floatingAddress ""
applyMaskFloating _ _ = error "applyMaskFloating received an unexpected input"

applyMaskBinaryFloating :: String -> String -> String
applyMaskBinaryFloating _ [] = []
applyMaskBinaryFloating [] x = x
applyMaskBinaryFloating (m:mask) (b:binaryNum)
  | m == '0' = b : applyMaskBinaryFloating mask binaryNum
  | otherwise = m : applyMaskBinaryFloating mask binaryNum

unfoldFloatingAddress :: String -> String -> [String]
unfoldFloatingAddress [] acc = [acc]
unfoldFloatingAddress (b:bs) acc
  | b == '0' || b == '1' = unfoldFloatingAddress bs (acc ++ [b])
  | b == 'X' =
    unfoldFloatingAddress bs (acc ++ "0") ++
    unfoldFloatingAddress bs (acc ++ "1")

fourteenthDecemberSolution2 :: IO Int
fourteenthDecemberSolution2 =
  sum . elems . fst . executeInstructionsV2 initialState . fmap parseInput <$>
  input

testInput :: String
testInput =
  "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
            \mem[8] = 11\n\
            \mem[7] = 101\n\
            \mem[8] = 0"

testInput2 :: String
testInput2 =
  "mask = 000000000000000000000000000000X1001X\n\
	     \mem[42] = 100\n\
	     \mask = 00000000000000000000000000000000X0XX\n\
	     \mem[26] = 1"
