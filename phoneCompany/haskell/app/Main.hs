module Main where

import Parsing.ParseInput
import Pure.Transformation
import Data.Map.Strict
import Pure.BusinessLogic

main :: IO ()
main = do
  calls <- parseCalls "calls.log"
  let result = (businessLogic . groupByCostumer) calls
  (putStrLn . foldMap (\t -> (show . fst) t ++ " - Total(GBP): " ++ (show . fromEnum . snd) t ++ "\n") . toList) result
