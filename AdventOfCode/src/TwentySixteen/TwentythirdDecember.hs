{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}

module TwentySixteen.TwentythirdDecember where

import System.Mem
import Data.Functor
import Control.Exception
import Control.Monad
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe
import Data.Sequence (Seq, fromList, index)
import qualified Data.Sequence as Seq (adjust', length)
import qualified TwentySixteen.TwelfthDecember as T

class TCInstruction a where
  parse :: String -> a
  toInstruction :: a -> Maybe T.Instruction
  toInstructionV2 :: a -> Maybe InstructionV2

data SafeInstruction = forall t. (TCInstruction t, Show t) => SafeInstruction t

data InstructionV2 =
  TGL String
  | Invalid SafeInstruction
  | ADD String String
  | MUL String String
  deriving (Show)

instance TCInstruction T.Instruction where
  parse = T.parseInstruction
  toInstruction x = Just x
  toInstructionV2 _ = Nothing

instance TCInstruction InstructionV2 where
  parse = parseInstructionV2
  toInstruction _ = Nothing
  toInstructionV2 x = Just x

instance TCInstruction SafeInstruction where
  parse _ = error "no need to parse a SafeInstruction"
  toInstruction (SafeInstruction x) = toInstruction x
  toInstructionV2 (SafeInstruction x) = toInstructionV2 x

instance Show SafeInstruction where
  show (SafeInstruction t) = show t

input :: IO (Seq SafeInstruction)
input = do
  i <- readFile "input/2016/23December.txt"
  parseInput i

parseInput :: String -> IO (Seq SafeInstruction)
parseInput i = do
  let is = lines i
  lis <- mapM parseSafeInstruction is
  return $ fromList lis

isInvalid :: InstructionV2 -> Bool
isInvalid (Invalid _) = True
isInvalid _ = False

parseInstructionV2 :: String -> InstructionV2
parseInstructionV2 s
  | "tgl " `isPrefixOf` s = TGL $ drop 4 s
  | "add " `isPrefixOf` s = ADD
      ( ( takeWhile (' ' /=)
          . drop 4) s
      )
      ((tail . dropWhile (' ' /=) . drop 4) s)
  | "mul " `isPrefixOf` s = MUL
      ( ( takeWhile (' ' /=)
          . drop 4) s
      )
      ((tail . dropWhile (' ' /=) . drop 4) s)
  | otherwise = error "not supported instruction"

parseSafeInstruction :: String -> IO SafeInstruction
parseSafeInstruction s = do
  mayInstruction <- catch ((fmap (Just . SafeInstruction) . evaluate) (parse @T.Instruction s)) handler
  mayInsV2 <- catch ((fmap (Just . SafeInstruction) . evaluate) (parse @InstructionV2 s)) handler
  (return . fromJust) $ msum [mayInstruction, mayInsV2]
  where
    handler :: SomeException -> IO (Maybe SafeInstruction)
    handler _ = return Nothing

registers1 :: Map String Int
registers1 = Map.fromList [("a", 7), ("b", 0), ("c", 0), ("d", 0)]

interpreter1 :: Seq SafeInstruction -> Int -> Map String Int -> IO (Map String Int)
interpreter1 is pointer regs
  | Seq.length is <= pointer = return regs
  | (foldr (\x _ -> isInvalid x) False . toInstructionV2) nextInstruction = interpreter1 is (pointer + 1) regs
  | (isJust . toInstructionV2) nextInstruction = do
    let indexToToggle = calculateIndexToToggle ((fromJust . toInstructionV2) nextInstruction) pointer regs
        instructions = Seq.adjust' toggle indexToToggle is
    print $ "indexToToggle " ++ show indexToToggle
    interpreter1 instructions (pointer + 1) regs
  | otherwise = do
    let (pointer', regs') = T.interpretInstruction ((fromJust . toInstruction) nextInstruction) pointer regs
    performMinorGC
    interpreter1 is pointer' regs'
  where
    nextInstruction = is `index` pointer

toggle :: SafeInstruction -> SafeInstruction
toggle (SafeInstruction i)
  | (isJust . toInstruction) i = toggleInstruction ((fromJust . toInstruction) i)
  | (foldr (\x _ -> isInvalid x) False . toInstructionV2) i = SafeInstruction i
  | (isJust . toInstructionV2) i = toggleTglInstruction ((fromJust . toInstructionV2) i)

toggleTglInstruction :: InstructionV2 -> SafeInstruction
toggleTglInstruction (TGL x) = SafeInstruction $ T.INC x
toggleTglInstruction _ = error "unexpected instruction"

toggleInstruction :: T.Instruction -> SafeInstruction
toggleInstruction (T.CPY vr r'       ) = SafeInstruction $ T.JNZ vr (Right r')
toggleInstruction (T.INC r           ) = SafeInstruction $ T.DEC r
toggleInstruction (T.DEC r           ) = SafeInstruction $ T.INC r
toggleInstruction (T.JNZ vr (Left v')) = SafeInstruction $ Invalid $ SafeInstruction $ T.CPY vr (show v')
toggleInstruction (T.JNZ vr (Right r)) = SafeInstruction $ T.CPY vr r

calculateIndexToToggle :: InstructionV2 -> Int -> Map String Int -> Int
calculateIndexToToggle (TGL r) pointer = (pointer + ) . fromJust . Map.lookup r
calculateIndexToToggle _ _ = error "unexpected instruction"

inputTest :: IO (Seq SafeInstruction)
inputTest = parseInput "cpy 2 a\n\
\tgl a\n\
\tgl a\n\
\tgl a\n\
\cpy 1 a\n\
\dec a\n\
\dec a"

testSolution1 :: IO Bool
testSolution1 =  inputTest >>= (`solution` registers1) <&> (== 3)

solution :: Seq SafeInstruction -> Map String Int -> IO Int
solution is = fmap (fromJust . Map.lookup "a") . interpreter1 is 0

twentythirdDecemberSolution1 :: IO Int
twentythirdDecemberSolution1 =  input >>= (`solution` registers1)

registers2 :: Map String Int
registers2 = Map.fromList [("a", 13), ("b", 0), ("c", 0), ("d", 0)]

input2 :: IO (Seq SafeInstruction)
input2 = do
  i <- readFile "input/2016/23DecemberP2.txt"
  parseInput i

twentythirdDecemberSolution2 :: IO Int
twentythirdDecemberSolution2 = undefined -- input >>= (`solution` registers2)
