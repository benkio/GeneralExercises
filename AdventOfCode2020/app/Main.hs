module Main where

import qualified EighthDecember  as Eighth
import qualified FifthDecember   as Fifth
import qualified FirstDecember   as First
import qualified FourthDecember  as Fourth
import qualified NinthDecember   as Ninth
import qualified SecondDecember  as Second
import qualified SeventhDecember as Seventh
import qualified SixthDecember   as Sixth
import qualified ThirdDecember   as Third

main :: IO ()
main = Ninth.ninthDecemberSolution2 >>= print
