module TwentyTwentyThree.Tests where

import TestCase (TestCase (..))
import qualified TwentyTwentyThree.December01 as S1
import qualified TwentyTwentyThree.December02 as S2
import qualified TwentyTwentyThree.December03 as S3
import qualified TwentyTwentyThree.December04 as S4
import qualified TwentyTwentyThree.December05 as S5
import qualified TwentyTwentyThree.December06 as S6
import qualified TwentyTwentyThree.December07 as S7
import qualified TwentyTwentyThree.December08 as S8
import qualified TwentyTwentyThree.December09 as S9
import qualified TwentyTwentyThree.December10 as S10
import qualified TwentyTwentyThree.December11 as S11
import qualified TwentyTwentyThree.December12 as S12
import qualified TwentyTwentyThree.December13 as S13
import qualified TwentyTwentyThree.December14 as S14
import qualified TwentyTwentyThree.December15 as S15
import qualified TwentyTwentyThree.December16 as S16
import qualified TwentyTwentyThree.December17 as S17
import qualified TwentyTwentyThree.December18 as S18
import qualified TwentyTwentyThree.December19 as S19
import qualified TwentyTwentyThree.December20 as S20
import qualified TwentyTwentyThree.December21 as S21
import qualified TwentyTwentyThree.December22 as S22
import qualified TwentyTwentyThree.December23 as S23
import qualified TwentyTwentyThree.December24 as S24
import qualified TwentyTwentyThree.December25 as S25

tests :: [TestCase]
tests =
    [       TestCase
        { testName = "TwentyTwentyThree-December01-solution1"
        , testAction = S1.december01Solution1
        , expectedResult = 56042
        }
    , TestCase
        { testName = "TwentyTwentyThree-December01-solution2"
        , testAction = S1.december01Solution2
        , expectedResult = 55358
        }
    , TestCase
        { testName = "TwentyTwentyThree-December02-solution1"
        , testAction = S2.december02Solution1
        , expectedResult = 2541
        }
    , TestCase
        { testName = "TwentyTwentyThree-December02-solution2"
        , testAction = S2.december02Solution2
        , expectedResult = 66016
        }
    , TestCase
        { testName = "TwentyTwentyThree-December03-solution1"
        , testAction = S3.december03Solution1
        , expectedResult = 528819
        }
    , TestCase
        { testName = "TwentyTwentyThree-December03-solution2"
        , testAction = S3.december03Solution2
        , expectedResult = 80403602
        }
    , TestCase
        { testName = "TwentyTwentyThree-December04-solution1"
        , testAction = S4.december04Solution1
        , expectedResult = 20117
        }
    , TestCase
        { testName = "TwentyTwentyThree-December04-solution2"
        , testAction = S4.december04Solution2
        , expectedResult = 13768818
        }
    , TestCase
        { testName = "TwentyTwentyThree-December05-solution1"
        , testAction = S5.december05Solution1
        , expectedResult = 322500873
        }
    , TestCase
        { testName = "TwentyTwentyThree-December05-solution2"
        , testAction = S5.december05Solution2
        , expectedResult = 108956227
        }
    , TestCase
        { testName = "TwentyTwentyThree-December06-solution1"
        , testAction = S6.december06Solution1
        , expectedResult = 2756160
        }
    , TestCase
        { testName = "TwentyTwentyThree-December06-solution2"
        , testAction = S6.december06Solution2
        , expectedResult = 34788142
        }
    , TestCase
        { testName = "TwentyTwentyThree-December07-solution1"
        , testAction = S7.december07Solution1
        , expectedResult = 251029473
        }
    , TestCase
        { testName = "TwentyTwentyThree-December07-solution2"
        , testAction = S7.december07Solution2
        , expectedResult = 251003917
        }
    , TestCase
        { testName = "TwentyTwentyThree-December08-solution1"
        , testAction = S8.december08Solution1
        , expectedResult = 16531
        }
    , TestCase
        { testName = "TwentyTwentyThree-December08-solution2"
        , testAction = S8.december08Solution2
        , expectedResult = 24035773251517
        }
    , TestCase
        { testName = "TwentyTwentyThree-December09-solution1"
        , testAction = S9.december09Solution1
        , expectedResult = 1581679977
        }
    , TestCase
        { testName = "TwentyTwentyThree-December09-solution2"
        , testAction = S9.december09Solution2
        , expectedResult = 889
        }
    , TestCase
        { testName = "TwentyTwentyThree-December10-solution1"
        , testAction = S10.december10Solution1
        , expectedResult = 1
        }
    , TestCase
        { testName = "TwentyTwentyThree-December10-solution2"
        , testAction = S10.december10Solution2
        , expectedResult = 381
        }
    , TestCase
        { testName = "TwentyTwentyThree-December11-solution1"
        , testAction = S11.december11Solution1
        , expectedResult = 9686930
        }
    , TestCase
        { testName = "TwentyTwentyThree-December11-solution2"
        , testAction = S11.december11Solution2
        , expectedResult = 630728425490
        }
    , TestCase
        { testName = "TwentyTwentyThree-December12-solution1"
        , testAction = S12.december12Solution1
        , expectedResult = 6958
        }
    , TestCase
        { testName = "TwentyTwentyThree-December12-solution2"
        , testAction = S12.december12Solution2
        , expectedResult = 6555315065024
        }
    , TestCase
        { testName = "TwentyTwentyThree-December14-solution1"
        , testAction = S14.december14Solution1
        , expectedResult = 109661
        }
    , TestCase
        { testName = "TwentyTwentyThree-December14-solution2"
        , testAction = S14.december14Solution2
        , expectedResult = 90176
        }
    , TestCase
        { testName = "TwentyTwentyThree-December15-solution1"
        , testAction = S15.december15Solution1
        , expectedResult = 506869
        }
    , TestCase
        { testName = "TwentyTwentyThree-December15-solution2"
        , testAction = S15.december15Solution2
        , expectedResult = 271384
        }
    , TestCase
        { testName = "TwentyTwentyThree-December16-solution1"
        , testAction = S16.december16Solution1
        , expectedResult = 6816
        }
    , TestCase
        { testName = "TwentyTwentyThree-December16-solution2"
        , testAction = S16.december16Solution2
        , expectedResult = 8163
        }
    , TestCase
        { testName = "TwentyTwentyThree-December17-solution1"
        , testAction = S17.december17Solution1
        , expectedResult = 1256
        }
    , TestCase
        { testName = "TwentyTwentyThree-December17-solution2"
        , testAction = S17.december17Solution2
        , expectedResult = 1382
        }
    , TestCase
        { testName = "TwentyTwentyThree-December18-solution1"
        , testAction = S18.december18Solution1
        , expectedResult = 49578
        }
    , TestCase
        { testName = "TwentyTwentyThree-December18-solution2"
        , testAction = S18.december18Solution2
        , expectedResult = 52885384955882
        }
    , TestCase
        { testName = "TwentyTwentyThree-December20-solution1"
        , testAction = S20.december20Solution1
        , expectedResult = 899848294
        }
    , TestCase
        { testName = "TwentyTwentyThree-December13-solution1"
        , testAction = S13.thirdteenthDecemberSolution1
        , expectedResult = 29846
        }
    , TestCase
        { testName = "TwentyTwentyThree-December13-solution2"
        , testAction = S13.thirdteenthDecemberSolution2
        , expectedResult = 25401
        }
    , TestCase
        { testName = "TwentyTwentyThree-December19-solution1"
        , testAction = S19.ninetheenthDecemberSolution1
        , expectedResult = 399284
        }
    , TestCase
        { testName = "TwentyTwentyThree-December19-solution2"
        , testAction = S19.ninetheenthDecemberSolution2
        , expectedResult = 121964982771486
        }
    , TestCase
        { testName = "TwentyTwentyThree-December21-solution1"
        , testAction = S21.twentyfirstDecemberSolution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwentyThree-December21-solution2"
        , testAction = S21.twentyfirstDecemberSolution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwentyThree-December22-solution1"
        , testAction = S22.twentysecondDecemberSolution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwentyThree-December22-solution2"
        , testAction = S22.twentysecondDecemberSolution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwentyThree-December23-solution1"
        , testAction = S23.twentythirdDecemberSolution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwentyThree-December24-solution1"
        , testAction = S24.twentyfourthDecemberSolution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwentyThree-December24-solution2"
        , testAction = S24.twentyfourthDecemberSolution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwentyThree-December25-solution1"
        , testAction = S25.twentyfifthDecemberSolution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwentyThree-December25-solution2"
        , testAction = S25.twentyfifthDecemberSolution2
        , expectedResult = undefined
        }
    ]
