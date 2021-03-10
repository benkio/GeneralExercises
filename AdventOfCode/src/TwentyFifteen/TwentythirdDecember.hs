module TwentyFifteen.TwentythirdDecember where

import Control.Monad.State
import Data.Vector (Vector, (!))
import Data.Vector as Vector (fromList, length)

data Instruction
  = HLF String
  | TPL String
  | INC String
  | JMP Int
  | JIE String Int
  | JIO String Int
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction i = parseInstruction' $ words i
  where
    parseInstruction' :: [String] -> Instruction
    parseInstruction' ["hlf", x] = HLF x
    parseInstruction' ["tpl", x] = TPL x
    parseInstruction' ["inc", x] = INC x
    parseInstruction' ["jmp", x] = JMP (toOffset x)
    parseInstruction' ["jie", x, offset] = JIE (init x) (toOffset offset)
    parseInstruction' ["jio", x, offset] = JIO (init x) (toOffset offset)
    parseInstruction' s = error $ "not recognized input: " ++ show s

toOffset :: String -> Int
toOffset ('+' : num) = read num :: Int
toOffset ('-' : num) = negate (read num :: Int)
toOffset s = error $ "offset not recognized: " ++ s

input :: IO (Vector Instruction)
input =
  Vector.fromList . fmap parseInstruction . lines
    <$> readFile "input/2015/23December.txt"

inputTest :: Vector Instruction
inputTest =
  (Vector.fromList . fmap parseInstruction . lines)
    "inc a\n\
    \jio a, +2\n\
    \tpl a\n\
    \inc a"

interpreter :: Int -> State (Int, Int, Vector Instruction) (Int, Int)
interpreter addr = do
  (a, b, instr) <- get
  let nextInstruction = instr ! addr
  let (a', b', addr') = interpretInstruction nextInstruction a b addr
  put (a', b', instr)
  if addr' < Vector.length instr && addr' >= 0
    then interpreter addr'
    else return (a', b')

interpretInstruction :: Instruction -> Int -> Int -> Int -> (Int, Int, Int)
interpretInstruction (HLF r) a b addr
  | r == "a" = (a `div` 2, b, addr + 1)
  | otherwise = (a, b `div` 2, addr + 1)
interpretInstruction (TPL r) a b addr
  | r == "a" = (a * 3, b, addr + 1)
  | otherwise = (a, b * 3, addr + 1)
interpretInstruction (INC r) a b addr
  | r == "a" = (a + 1, b, addr + 1)
  | otherwise = (a, b + 1, addr + 1)
interpretInstruction (JMP offset) a b addr = (a, b, addr + offset)
interpretInstruction (JIE r offset) a b addr
  | r == "a" && even a = (a, b, addr + offset)
  | r == "b" && even b = (a, b, addr + offset)
  | otherwise = (a, b, addr + 1)
interpretInstruction (JIO r offset) a b addr
  | r == "a" && a == 1 = (a, b, addr + offset)
  | r == "b" && b == 1 = (a, b, addr + offset)
  | otherwise = (a, b, addr + 1)

solution1 :: Vector Instruction -> (Int, Int)
solution1 instr = evalState (interpreter 0) (0, 0, instr)

solution1Test :: Bool
solution1Test = (fst . solution1) inputTest == 2

twentythirdDecemberSolution1 :: IO Int
twentythirdDecemberSolution1 = snd . solution1 <$> input

solution2 :: Vector Instruction -> (Int, Int)
solution2 instr = evalState (interpreter 0) (1, 0, instr)

twentythirdDecemberSolution2 :: IO Int
twentythirdDecemberSolution2 = snd . solution2 <$> input
