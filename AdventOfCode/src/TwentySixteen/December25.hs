module TwentySixteen.December25 where

import Control.Exception
import Control.Monad
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map (adjust, empty, fromList, insert, toList, union)
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq (fromList, index)
import qualified TwentySixteen.December12 as T

data BunnyAssembly
    = B1 T.Instruction
    | Out String
    deriving (Show)

type Registers = Map String Int

input :: IO (Seq BunnyAssembly)
input = do
    i <- readFile "input/2016/25December.txt"
    (fmap Seq.fromList . traverse parseBunnyAssembly . lines) i

parseBunnyAssembly :: String -> IO BunnyAssembly
parseBunnyAssembly s = do
    mayInstruction <- catch ((fmap (Just . B1) . evaluate) (T.parseInstruction s)) handler
    mayOut <- (return . parseOut) s
    (return . fromJust) $ msum [mayInstruction, mayOut]
  where
    handler :: SomeException -> IO (Maybe BunnyAssembly)
    handler _ = return Nothing

parseOut :: String -> Maybe BunnyAssembly
parseOut s
    | "out " `isPrefixOf` s = (Just . Out . drop 4) s
    | otherwise = Nothing

emitSignal :: Registers -> Int -> Seq BunnyAssembly -> [Int]
emitSignal regs pointer instructions
    | isJust mayOutInstructionRegister = regs ! fromJust mayOutInstructionRegister : emitSignal regs (pointer + 1) instructions
    | otherwise =
        let (pointer', regs') = T.interpretInstruction ((fromJust . oldInstructionValue) currentInstruction) pointer regs
         in emitSignal regs' pointer' instructions
  where
    currentInstruction = Seq.index instructions pointer
    mayOutInstructionRegister = outValue currentInstruction

outValue :: BunnyAssembly -> Maybe String
outValue (Out r) = Just r
outValue _ = Nothing

oldInstructionValue :: BunnyAssembly -> Maybe T.Instruction
oldInstructionValue (B1 i) = Just i
oldInstructionValue _ = Nothing

registers :: Int -> Map String Int
registers a = Map.fromList [("a", a), ("b", 0), ("c", 0), ("d", 0)]

targetSignal :: [Int]
targetSignal = iterate (\x -> if x == 0 then 1 else 0) 0

solution :: Seq BunnyAssembly -> Int -> Int
solution instructions a
    | take 100 l == take 100 targetSignal = a
    | otherwise = solution instructions (a + 1)
  where
    l = emitSignal (registers a) 0 instructions

twentyfifthDecemberSolution :: IO Int
twentyfifthDecemberSolution = (`solution` 0) <$> input
