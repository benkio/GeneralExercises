module Main where

import qualified FirstDecember as First
import qualified SecondDecember as Second
import qualified ThirdDecember as Third

main :: IO ()
main = Third.thirdDecemberSolution2 >>= print
