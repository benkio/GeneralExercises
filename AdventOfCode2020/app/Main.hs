module Main where

import qualified FirstDecember as First
import qualified SecondDecember as Second

main :: IO ()
main = Second.solution2 >>= print
