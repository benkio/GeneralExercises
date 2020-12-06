module Main where

import qualified FifthDecember  as Fifth
import qualified FirstDecember  as First
import qualified FourthDecember as Fourth
import qualified SecondDecember as Second
import qualified SixthDecember  as Sixth
import qualified ThirdDecember  as Third

main :: IO ()
main = Sixth.sixthDecemberSolution2 >>= print
