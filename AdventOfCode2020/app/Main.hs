module Main where

import qualified FirstDecember  as First
import qualified FourthDecember as Fourth
import qualified SecondDecember as Second
import qualified ThirdDecember  as Third

main :: IO ()
main = Fourth.fourthDecemberSolution1 >>= print
