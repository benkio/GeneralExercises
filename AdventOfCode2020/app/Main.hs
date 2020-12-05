module Main where

import qualified FifthDecember as Fifth
import qualified FirstDecember as First
import qualified FourthDecember as Fourth
import qualified SecondDecember as Second
import qualified ThirdDecember as Third

main :: IO ()
main = Fifth.fifthDecemberSolution2 >>= print
