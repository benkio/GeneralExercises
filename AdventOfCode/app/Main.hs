module Main where

import qualified TwentyTwenty.EighthDecember      as Eight
import qualified TwentyTwenty.EleventhDecember    as Eleven
import qualified TwentyTwenty.FifteenthDecember   as Fifteen
import qualified TwentyTwenty.FifthDecember       as Five
import qualified TwentyTwenty.FirstDecember       as One
import qualified TwentyTwenty.FourteenthDecember  as Fourteen
import qualified TwentyTwenty.FourthDecember      as Four
import qualified TwentyTwenty.NinthDecember       as Nine
import qualified TwentyTwenty.SecondDecember      as Two
import qualified TwentyTwenty.SeventeenthDecember as Seventeen
import qualified TwentyTwenty.SeventhDecember     as Seven
import qualified TwentyTwenty.SixteenthDecember   as Sixteen
import qualified TwentyTwenty.SixthDecember       as Six
import qualified TwentyTwenty.TenthDecember       as Ten
import qualified TwentyTwenty.ThirdDecember       as Three
import qualified TwentyTwenty.ThirteenthDecember  as Thirteen
import qualified TwentyTwenty.TwelfthDecember     as Twelve

main :: IO ()
main = Seventeen.seventeenthDecemberSolution2 >>= print
