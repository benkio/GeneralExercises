{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
module TwentySixteen.TwentythirdDecember where

import Control.Exception
import Data.Maybe
import Control.Monad
import Data.List
import qualified TwentySixteen.TwelfthDecember as T

class ParsableInstruction a where
  parse :: String -> a

data SafeInstruction = forall t. (ParsableInstruction t, Show t) => SafeInstruction t
data TGL = TGL String deriving Show

instance ParsableInstruction T.Instruction where
  parse = T.parseInstruction

instance ParsableInstruction TGL where
  parse s
    | "tgl " `isPrefixOf` s = TGL $ drop 4 s
    | otherwise = error "no TGL instruction"

instance Show SafeInstruction where
  show (SafeInstruction t) = show t

input :: IO [SafeInstruction]
input = do
  i <- readFile "input/2016/23December.txt"
  let is = lines i
  mapM parseSafeInstruction is

parseSafeInstruction :: String -> IO SafeInstruction
parseSafeInstruction s = do
  mayInstruction <- catch ((fmap (Just . SafeInstruction) . evaluate) (parse @T.Instruction s)) handler
  mayTgl <- catch ((fmap (Just . SafeInstruction) . evaluate) (parse @TGL s)) handler
  (return . fromJust) $ msum [mayInstruction, mayTgl]
  where handler :: SomeException -> IO (Maybe SafeInstruction)
        handler _ = return Nothing

solution1 :: String -> Int
solution1 = undefined

twentythirdDecemberSolution1 :: IO Int
twentythirdDecemberSolution1 = undefined

solution2 :: String -> Int
solution2 = undefined

twentythirdDecemberSolution2 :: IO Int
twentythirdDecemberSolution2 = undefined
