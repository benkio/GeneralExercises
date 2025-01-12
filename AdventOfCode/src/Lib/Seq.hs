module Lib.Seq (
    rotate,
) where

import Data.Sequence (Seq, cycleTaking)
import qualified Data.Sequence as S (drop, length)

rotate :: Int -> Seq a -> Seq a
rotate times xs = S.drop times . cycleTaking (S.length xs + times) $ xs
