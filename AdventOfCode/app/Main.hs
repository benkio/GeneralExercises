module Main where

import qualified TwentyTwenty.EighthDecember   as Eighth
import qualified TwentyTwenty.FifthDecember    as Fifth
import qualified TwentyTwenty.FirstDecember    as First
import qualified TwentyTwenty.FourthDecember   as Fourth
import qualified TwentyTwenty.NinthDecember    as Ninth
import qualified TwentyTwenty.SecondDecember   as Second
import qualified TwentyTwenty.SeventhDecember  as Seventh
import qualified TwentyTwenty.SixthDecember    as Sixth
import qualified TwentyTwenty.TenthDecember    as Tenth
import qualified TwentyTwenty.ThirdDecember    as Third
import qualified TwentyTwenty.TwelfthDecember  as Twelfth

import qualified TwentyTwenty.EleventhDecember as Eleventh
main :: IO ()
main = Twelfth.twelfthDecemberSolution2 >>= print
