module TwentyTwentyOne.TwentyFourthDecember where

import Text.Read

data State = State
  { w :: Int,
    x :: Int,
    y :: Int,
    z :: Int
  }

data Instruction
  = InputInstruction String
  | Instruction String String (Either String Int)
  deriving Show

parseInput :: String -> [Instruction]
parseInput = fmap parseInstruction . lines
  where
    parseInstruction =
      ( \s -> case (s !! 0) of
          "inp" -> InputInstruction (s !! 1)
          _ -> Instruction (s !! 0) (s !! 1) $ foldl (\_ v -> Right v) (Left (s !! 2)) (readMaybe (s !! 2) :: Maybe Int)
      )
        . words

-- Silly Idea:
--   take the last block, provide it with w = [1..9] and z = [-1000..1000]
--   see the first occurrence of w (1..9) and relative z.
--   repeat the procedure for the block above and check if the value of Z matches
--   repeat until you reach the initial block

input :: IO String
input = readFile "input/2021/24December.txt"

twentyFourthDecemberSolution1 :: IO Int
twentyFourthDecemberSolution1 = undefined

twentyFourthDecemberSolution2 :: IO Int
twentyFourthDecemberSolution2 = undefined
