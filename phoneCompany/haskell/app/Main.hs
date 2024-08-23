module Main where

import Data.Map.Strict
import Parsing.ParseInput
import Pure.BusinessLogic
import Pure.Transformation

main :: IO ()
main = do
    calls <- parseCalls "calls.log"
    let result = (businessLogic . groupByCostumer) calls
    (putStrLn . foldMap (\t -> (show . fst) t ++ " - Total(GBP): " ++ (show . fromEnum . snd) t ++ "\n") . toList) result
