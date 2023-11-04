{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}

module TwentySixteen.TwentythirdDecember where

import Control.Exception
import Control.Monad
import Data.Functor
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map (adjust, fromList, lookup)
import Data.Maybe
import Data.Sequence (Seq, fromList, index)
import qualified Data.Sequence as Seq (adjust', length)
import System.Mem
import qualified TwentySixteen.TwelfthDecember as T

class TCInstruction a where
    parse :: String -> a
    toInstruction :: a -> Maybe T.Instruction
    toInstructionV2 :: a -> Maybe InstructionV2

data SafeInstruction = forall t. (TCInstruction t, Show t) => SafeInstruction t

data InstructionV2
    = TGL String
    | Invalid SafeInstruction
    | ADD String String
    | MUL String String
    deriving (Show)

instance TCInstruction T.Instruction where
    parse = T.parseInstruction
    toInstruction = Just
    toInstructionV2 _ = Nothing

instance TCInstruction InstructionV2 where
    parse = parseInstructionV2
    toInstruction _ = Nothing
    toInstructionV2 = Just

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
    | "add " `isPrefixOf` s =
        ADD
            ( ( takeWhile (' ' /=)
                    . drop 4
              )
                s
            )
            ((tail . dropWhile (' ' /=) . drop 4) s)
    | "mul " `isPrefixOf` s =
        MUL
            ( ( takeWhile (' ' /=)
                    . drop 4
              )
                s
            )
            ((tail . dropWhile (' ' /=) . drop 4) s)
    | otherwise = Invalid $ SafeInstruction $ TGL "not used"

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

toggle :: SafeInstruction -> SafeInstruction
toggle (SafeInstruction i)
    | (isJust . toInstruction) i = toggleInstruction ((fromJust . toInstruction) i)
    | (foldr (\x _ -> isInvalid x) False . toInstructionV2) i = SafeInstruction i
    | (isJust . toInstructionV2) i = toggleTglInstruction ((fromJust . toInstructionV2) i)

toggleTglInstruction :: InstructionV2 -> SafeInstruction
toggleTglInstruction (TGL x) = SafeInstruction $ T.INC x
toggleTglInstruction _ = error "unexpected instruction"

toggleInstruction :: T.Instruction -> SafeInstruction
toggleInstruction (T.CPY vr r') = SafeInstruction $ T.JNZ vr (Right r')
toggleInstruction (T.INC r) = SafeInstruction $ T.DEC r
toggleInstruction (T.DEC r) = SafeInstruction $ T.INC r
toggleInstruction (T.JNZ vr (Left v')) = SafeInstruction $ Invalid $ SafeInstruction $ T.CPY vr (show v')
toggleInstruction (T.JNZ vr (Right r)) = SafeInstruction $ T.CPY vr r

calculateIndexToToggle :: InstructionV2 -> Int -> Map String Int -> Int
calculateIndexToToggle (TGL r) pointer = (pointer +) . fromJust . Map.lookup r
calculateIndexToToggle _ _ = error "unexpected instruction"

inputTest :: IO (Seq SafeInstruction)
inputTest =
    parseInput
        "cpy 2 a\n\
        \tgl a\n\
        \tgl a\n\
        \tgl a\n\
        \cpy 1 a\n\
        \dec a\n\
        \dec a"

testSolution1 :: IO Bool
testSolution1 = inputTest >>= (`solution` registers1) <&> (== 3)

solution :: Seq SafeInstruction -> Map String Int -> IO Int
solution is = fmap (fromJust . Map.lookup "a") . interpreter2 is 0

twentythirdDecemberSolution1 :: IO Int
twentythirdDecemberSolution1 = input >>= (`solution` registers1)

registers2 :: Map String Int
registers2 = Map.fromList [("a", 12), ("b", 0), ("c", 0), ("d", 0)]

input2 :: IO (Seq SafeInstruction)
input2 = do
    i <- readFile "input/2016/23DecemberP2.txt"
    parseInput i

interpreter2 :: Seq SafeInstruction -> Int -> Map String Int -> IO (Map String Int)
interpreter2 is pointer regs
    | Seq.length is <= pointer = return regs
    | (isJust . toInstructionV2) nextInstruction =
        let (is', pointer', regs') = interpretInstructionV2 ((fromJust . toInstructionV2) nextInstruction) is pointer regs
         in interpreter2 is' pointer' regs'
    | otherwise = do
        let (pointer', regs') = T.interpretInstruction ((fromJust . toInstruction) nextInstruction) pointer regs
        performMinorGC
        interpreter2 is pointer' regs'
  where
    nextInstruction = is `index` pointer

interpretInstructionV2 :: InstructionV2 -> Seq SafeInstruction -> Int -> Map String Int -> (Seq SafeInstruction, Int, Map String Int)
interpretInstructionV2 i@(TGL _) is pointer regs =
    let indexToToggle = calculateIndexToToggle i pointer regs
        instructions = Seq.adjust' toggle indexToToggle is
     in (instructions, pointer + 1, regs)
interpretInstructionV2 (Invalid _) is pointer regs = (is, pointer + 1, regs)
interpretInstructionV2 (ADD r1 r2) is pointer regs =
    let firstRegister = (fromJust . Map.lookup r1) regs
     in (is, pointer + 1, Map.adjust (firstRegister +) r2 regs)
interpretInstructionV2 (MUL r1 r2) is pointer regs =
    let firstRegister = (fromJust . Map.lookup r1) regs
     in (is, pointer + 1, Map.adjust (firstRegister *) r2 regs)

twentythirdDecemberSolution2 :: IO Int
twentythirdDecemberSolution2 = input2 >>= (`solution` registers2)

-- 479009040
