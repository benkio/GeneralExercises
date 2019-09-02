module Main where

import Parsing.ParseInput
import Pure.Transformation
import Pure.BusinessLogic

main :: IO ()
main = do
  calls <- parseCalls "calls.log"
  let result = (businessLogic . groupByCostumer) calls
  putStrLn $ show result
