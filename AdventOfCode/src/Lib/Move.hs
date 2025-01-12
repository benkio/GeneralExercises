module Lib.Move (Move (..)) where

data Move = U | D | L | R deriving (Eq, Ord)

instance Show Move where
    show U = "^"
    show D = "v"
    show L = "<"
    show R = ">"
