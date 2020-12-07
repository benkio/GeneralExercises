module Main where

import qualified FifthDecember   as Fifth
import qualified FirstDecember   as First
import qualified FourthDecember  as Fourth
import qualified SecondDecember  as Second
import qualified SeventhDecember as Seventh
import qualified SixthDecember   as Sixth
import qualified ThirdDecember   as Third

main :: IO ()
main = Seventh.seventhDecemberSolution2 >>= print
