module Main where

import qualified TwentyTwenty.EighteenthDecember   as Eighteen
import qualified TwentyTwenty.EighthDecember       as Eight
import qualified TwentyTwenty.EleventhDecember     as Eleven
import qualified TwentyTwenty.FifteenthDecember    as Fifteen
import qualified TwentyTwenty.FifthDecember        as Five
import qualified TwentyTwenty.FirstDecember        as One
import qualified TwentyTwenty.FourteenthDecember   as Fourteen
import qualified TwentyTwenty.FourthDecember       as Four
import qualified TwentyTwenty.NineteenthDecember   as Nineteen
import qualified TwentyTwenty.NinthDecember        as Nine
import qualified TwentyTwenty.SecondDecember       as Two
import qualified TwentyTwenty.SeventeenthDecember  as Seventeen
import qualified TwentyTwenty.SeventhDecember      as Seven
import qualified TwentyTwenty.SixteenthDecember    as Sixteen
import qualified TwentyTwenty.SixthDecember        as Six
import qualified TwentyTwenty.TenthDecember        as Ten
import qualified TwentyTwenty.ThirdDecember        as Three
import qualified TwentyTwenty.ThirteenthDecember   as Thirteen
import qualified TwentyTwenty.TwelfthDecember      as Twelve
import qualified TwentyTwenty.TwentiethDecember    as Twenty
import qualified TwentyTwenty.TwentyFifthDecember  as TwentyFifth
import qualified TwentyTwenty.TwentyFirstDecember  as TwentyFirst
import qualified TwentyTwenty.TwentyFourthDecember as TwentyFourth
import qualified TwentyTwenty.TwentySecondDecember as TwentySecond
import qualified TwentyTwenty.TwentyThirdDecember  as TwentyThird

main :: IO ()
main = TwentyFifth.twentyFifthDecemberSolution1 >>= print
