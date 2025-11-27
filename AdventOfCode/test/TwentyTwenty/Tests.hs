module TwentyTwenty.Tests where

import TestCase (TestCase (..))
import qualified TwentyTwenty.December01 as S1
import qualified TwentyTwenty.December02 as S2
import qualified TwentyTwenty.December03 as S3
import qualified TwentyTwenty.December04 as S4
import qualified TwentyTwenty.December05 as S5
import qualified TwentyTwenty.December06 as S6
import qualified TwentyTwenty.December07 as S7
import qualified TwentyTwenty.December08 as S8
import qualified TwentyTwenty.December09 as S9
import qualified TwentyTwenty.December10 as S10
import qualified TwentyTwenty.December11 as S11
import qualified TwentyTwenty.December12 as S12
import qualified TwentyTwenty.December13 as S13
import qualified TwentyTwenty.December14 as S14
import qualified TwentyTwenty.December15 as S15
import qualified TwentyTwenty.December16 as S16
import qualified TwentyTwenty.December17 as S17
import qualified TwentyTwenty.December18 as S18
import qualified TwentyTwenty.December19 as S19
import qualified TwentyTwenty.December20 as S20
import qualified TwentyTwenty.December21 as S21
import qualified TwentyTwenty.December22 as S22
import qualified TwentyTwenty.December24 as S24
import qualified TwentyTwenty.December25 as S25

tests :: [TestCase]
tests =
    [       TestCase
        { testName = "TwentyTwenty-December01-solution1"
        , testAction = S1.december01Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December01-solution2"
        , testAction = S1.december01Solution2
        , expectedResult = undefined
        },
          TestCase
        { testName = "TwentyTwenty-December02-solution1"
        , testAction = S2.december02Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December02-solution2"
        , testAction = S2.december02Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December03-solution1"
        , testAction = S3.december03Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December03-solution2"
        , testAction = S3.december03Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December04-solution1"
        , testAction = S4.december04Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December05-solution1"
        , testAction = S5.december05Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December05-solution2"
        , testAction = S5.december05Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December06-solution1"
        , testAction = S6.december06Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December06-solution2"
        , testAction = S6.december06Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December07-solution1"
        , testAction = S7.december07Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December07-solution2"
        , testAction = S7.december07Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December08-solution1"
        , testAction = S8.december08Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December08-solution2"
        , testAction = S8.december08Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December09-solution1"
        , testAction = S9.december09Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December09-solution2"
        , testAction = S9.december09Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December10-solution1"
        , testAction = S10.december10Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December10-solution2"
        , testAction = S10.december10Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December11-solution1"
        , testAction = S11.december11Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December11-solution2"
        , testAction = S11.december11Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December12-solution1"
        , testAction = S12.december12Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December12-solution2"
        , testAction = S12.december12Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December13-solution1"
        , testAction = S13.december13Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December13-solution2"
        , testAction = S13.december13Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December14-solution1"
        , testAction = S14.december14Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December14-solution2"
        , testAction = S14.december14Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December15-solution1"
        , testAction = S15.december15Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December15-solution2"
        , testAction = S15.december15Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December16-solution1"
        , testAction = S16.december16Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December16-solution2"
        , testAction = S16.december16Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December17-solution1"
        , testAction = S17.december17Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December17-solution2"
        , testAction = S17.december17Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December18-solution1"
        , testAction = S18.december18Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December18-solution2"
        , testAction = S18.december18Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December19-solution1"
        , testAction = S19.december19Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December19-solution2"
        , testAction = S19.december19Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December20-solution1"
        , testAction = S20.december20Solution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December20-solution2"
        , testAction = S20.december20Solution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December21-solution1"
        , testAction = S21.twentyFirstDecemberSolution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December21-solution2"
        , testAction = S21.twentyFirstDecemberSolution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December22-solution1"
        , testAction = S22.twentySecondDecemberSolution1
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December22-solution2"
        , testAction = S22.twentySecondDecemberSolution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December24-solution2"
        , testAction = S24.twentyFourthDecemberSolution2
        , expectedResult = undefined
        }
    , TestCase
        { testName = "TwentyTwenty-December25-solution1"
        , testAction = S25.twentyFifthDecemberSolution1
        , expectedResult = undefined
        }
    ]
