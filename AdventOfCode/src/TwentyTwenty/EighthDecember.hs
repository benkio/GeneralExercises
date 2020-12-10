{-# LANGUAGE ViewPatterns #-}

-------------------------------------------------------------------------------
--                           Advent Of Code - day 8                          --
-------------------------------------------------------------------------------
module TwentyTwenty.EighthDecember where

import Data.List (nub, stripPrefix)

data Instruction
  = Nop Int Int
  | Jump Int Int
  | Acc Int Int
  deriving (Show, Eq)

instructionPosition :: Instruction -> Int
instructionPosition (Nop _ ip) = ip
instructionPosition (Jump _ ip) = ip
instructionPosition (Acc _ ip) = ip

instructionValue :: Instruction -> Int
instructionValue (Nop v _) = v
instructionValue (Jump v _) = v
instructionValue (Acc v _) = v

stripPlus :: String -> String
stripPlus ('+':s) = s
stripPlus s = s

switchNopNJump :: Instruction -> Instruction
switchNopNJump (Nop v p) = Jump v p
switchNopNJump (Jump v p) = Nop v p
switchNopNJump x = x

interpreter :: Int -> Instruction -> (Int, Int)
interpreter acc (Nop _ p) = (acc, p + 1)
interpreter acc (Acc v p) = (acc + v, p + 1)
interpreter acc (Jump j p) = (acc, p + j)

replaceInstruction :: [Instruction] -> Instruction -> [Instruction]
replaceInstruction is i =
  let (prevI, nextI) = splitAt (instructionPosition i) is
      nextI' =
        if null nextI
          then []
          else tail nextI
   in prevI ++ [i] ++ nextI'

parseInstruction :: Int -> String -> Instruction
parseInstruction instructionPos (stripPrefix "jmp " -> Just steps) =
  Jump (read (stripPlus steps) :: Int) instructionPos
parseInstruction instructionPos (stripPrefix "acc " -> Just value) =
  Acc (read (stripPlus value) :: Int) instructionPos
parseInstruction instructionPos (stripPrefix "nop " -> Just value) =
  Nop (read (stripPlus value) :: Int) instructionPos
parseInstruction p _ = Nop 0 p

input :: IO [Instruction]
input =
  fmap (uncurry parseInstruction) . zip [0 ..] . lines <$>
  readFile "input/2020/8December.txt"

loopDetector :: (Int, [Instruction]) -> Int -> [Instruction] -> (Int, Int)
loopDetector (acc, instructionTrace) pointer is
  | pointer > instructionPosition (last is) = (acc, pointer)
  | i `elem` instructionTrace =
    (acc, foldl (\x y -> x `max` instructionPosition y) 0 instructionTrace)
  | otherwise =
    let (acc', pointer') = interpreter acc i
     in loopDetector (acc', instructionTrace ++ [i]) pointer' is
  where
    i = is !! pointer

fixLoop :: [Instruction] -> [(Int, Int)]
fixLoop is =
  filter (\(_, p) -> p == (instructionPosition (last is) + 1)) .
  fmap (loopDetector (0, []) 0) . nub $
  fmap (replaceInstruction is . switchNopNJump) is

eighthDecemberSolution1 :: IO Int
eighthDecemberSolution1 = fst . loopDetector (0, []) 0 <$> input

eighthDecemberSolution2 :: IO Int
eighthDecemberSolution2 = fst . head . fixLoop <$> input
