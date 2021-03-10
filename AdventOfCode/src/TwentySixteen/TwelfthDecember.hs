module TwentySixteen.TwelfthDecember where

import Data.List
import Data.Map (Map, adjust)
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector (fromList)
import Text.Read

data Instruction
  = CPY (Either Int String) String
  | INC String
  | DEC String
  | JNZ (Either Int String) Int
  deriving (Show)

registers :: Map String Int
registers = Map.fromList [("a", 0), ("b", 0), ("c", 0), ("d", 0)]

input :: IO (Vector Instruction)
input =
  Vector.fromList . fmap parseInstruction . lines
    <$> readFile "input/2016/12December.txt"

parseInstruction :: String -> Instruction
parseInstruction s
  | "inc " `isPrefixOf` s = INC $ drop 4 s
  | "dec " `isPrefixOf` s = DEC $ drop 4 s
  | "cpy " `isPrefixOf` s =
    CPY
      ( ( (\x -> maybe (Right x) Left (readMaybe x :: Maybe Int))
            . takeWhile (' ' /=)
            . drop 4
        )
          s
      )
      ((tail . dropWhile (' ' /=) . drop 4) s)
  | "jnz " `isPrefixOf` s =
    JNZ
      ( ( (\x -> maybe (Right x) Left (readMaybe x :: Maybe Int))
            . takeWhile (' ' /=)
            . drop 4
        )
          s
      )
      (((\x -> read x :: Int) . dropWhile (' ' /=) . drop 4) s)

interpreter1 :: Vector Instruction -> Int -> Map String Int -> Map String Int
interpreter1 is pointer regs
  | length is <= pointer = regs
  | otherwise =
    let (pointer', regs') = interpretInstruction (is ! pointer) pointer regs
     in interpreter1 is pointer' regs'

interpretInstruction ::
  Instruction -> Int -> Map String Int -> (Int, Map String Int)
interpretInstruction (INC r) pointer regs = (pointer + 1, adjust (+ 1) r regs)
interpretInstruction (DEC r) pointer regs =
  (pointer + 1, adjust (\x -> x - 1) r regs)
interpretInstruction (CPY (Left v) r) pointer regs =
  (pointer + 1, adjust (const v) r regs)
interpretInstruction (CPY (Right r') r) pointer regs =
  (pointer + 1, adjust (const ((fromJust . Map.lookup r') regs)) r regs)
interpretInstruction (JNZ (Left v) p) pointer regs
  | v /= 0 = (pointer + p, regs)
  | otherwise = (pointer + 1, regs)
interpretInstruction (JNZ (Right r') p) pointer regs
  | (fromJust . Map.lookup r') regs /= 0 = (pointer + p, regs)
  | otherwise = (pointer + 1, regs)

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 =
  fromJust . Map.lookup "a" . (\is -> interpreter1 is 0 registers) <$> input

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 =
  fromJust
    . Map.lookup "a"
    . (\is -> interpreter1 is 0 (adjust (const 1) "c" registers))
    <$> input
