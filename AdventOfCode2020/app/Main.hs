module Main where

import qualified FirstDecember as First
import qualified SecondDecember as Second
import qualified ThirdDecember as Third
import qualified ForthDecember as Forth

main :: IO ()
main = Forth.forthDecemberSolution1 >>= print
