module TwentyTwentyOne.Tests where

import TestCase (TestCase (..))
import qualified TwentyTwentyOne.December01 as S1
import qualified TwentyTwentyOne.December02 as S2
import qualified TwentyTwentyOne.December03 as S3
import qualified TwentyTwentyOne.December04 as S4
import qualified TwentyTwentyOne.December05 as S5
import qualified TwentyTwentyOne.December06 as S6
import qualified TwentyTwentyOne.December07 as S7
import qualified TwentyTwentyOne.December08 as S8
import qualified TwentyTwentyOne.December09 as S9
import qualified TwentyTwentyOne.December10 as S10
import qualified TwentyTwentyOne.December11 as S11
import qualified TwentyTwentyOne.December12 as S12
import qualified TwentyTwentyOne.December13 as S13
import qualified TwentyTwentyOne.December14 as S14
import qualified TwentyTwentyOne.December15 as S15
import qualified TwentyTwentyOne.December16 as S16
import qualified TwentyTwentyOne.December17 as S17
import qualified TwentyTwentyOne.December18 as S18
import qualified TwentyTwentyOne.December19 as S19
import qualified TwentyTwentyOne.December20 as S20
import qualified TwentyTwentyOne.December21 as S21
import qualified TwentyTwentyOne.December22 as S22
import qualified TwentyTwentyOne.December23P1 as S23
-- import qualified TwentyTwentyOne.December23P2 as S23P2
import qualified TwentyTwentyOne.December24 as S24
import qualified TwentyTwentyOne.December25 as S25

tests :: [TestCase]
tests =
    [       TestCase
        { testName = "TwentyTwentyOne-December01-solution1"
        , testAction = S1.december01Solution1
        , expectedResult = 1167
        }
    , TestCase
        { testName = "TwentyTwentyOne-December01-solution2"
        , testAction = S1.december01Solution2
        , expectedResult = 1130
        }
    , TestCase
        { testName = "TwentyTwentyOne-December02-solution1"
        , testAction = S2.december02Solution1
        , expectedResult = 1480518
        }
    , TestCase
        { testName = "TwentyTwentyOne-December02-solution2"
        , testAction = S2.december02Solution2
        , expectedResult = 1282809906
        }
    , TestCase
        { testName = "TwentyTwentyOne-December03-solution1"
        , testAction = S3.december03Solution1
        , expectedResult = 4001724
        }
    , TestCase
        { testName = "TwentyTwentyOne-December03-solution2"
        , testAction = S3.december03Solution2
        , expectedResult = 587895
        }
    , TestCase
        { testName = "TwentyTwentyOne-December04-solution1"
        , testAction = S4.december04Solution1
        , expectedResult = 12796
        }
    , TestCase
        { testName = "TwentyTwentyOne-December04-solution2"
        , testAction = S4.december04Solution2
        , expectedResult = 18063
        }
    , TestCase
        { testName = "TwentyTwentyOne-December05-solution1"
        , testAction = S5.december05Solution1
        , expectedResult = 4993
        }
    , TestCase
        { testName = "TwentyTwentyOne-December05-solution2"
        , testAction = S5.december05Solution2
        , expectedResult = 21101
        }
    , TestCase
        { testName = "TwentyTwentyOne-December06-solution1"
        , testAction = S6.december06Solution1
        , expectedResult = 386640
        }
    , TestCase
        { testName = "TwentyTwentyOne-December06-solution2"
        , testAction = S6.december06Solution2
        , expectedResult = 1733403626279
        }
    , TestCase
        { testName = "TwentyTwentyOne-December07-solution1"
        , testAction = S7.december07Solution1
        , expectedResult = 352254
        }
    , TestCase
        { testName = "TwentyTwentyOne-December07-solution2"
        , testAction = S7.december07Solution2
        , expectedResult = 99053143
        }
    , TestCase
        { testName = "TwentyTwentyOne-December08-solution1"
        , testAction = S8.december08Solution1
        , expectedResult = 362
        }
    , TestCase
        { testName = "TwentyTwentyOne-December08-solution2"
        , testAction = S8.december08Solution2
        , expectedResult = 1020159
        }
    , TestCase
        { testName = "TwentyTwentyOne-December09-solution1"
        , testAction = S9.december09Solution1
        , expectedResult = 448
        }
    , TestCase
        { testName = "TwentyTwentyOne-December09-solution2"
        , testAction = S9.december09Solution2
        , expectedResult = 1417248
        }
    , TestCase
        { testName = "TwentyTwentyOne-December10-solution1"
        , testAction = S10.december10Solution1
        , expectedResult = 294195
        }
    , TestCase
        { testName = "TwentyTwentyOne-December10-solution2"
        , testAction = S10.december10Solution2
        , expectedResult = 3490802734
        }
    , TestCase
        { testName = "TwentyTwentyOne-December11-solution1"
        , testAction = S11.december11Solution1
        , expectedResult = 1691
        }
    , TestCase
        { testName = "TwentyTwentyOne-December11-solution2"
        , testAction = S11.december11Solution2
        , expectedResult = 216
        }
    , TestCase
        { testName = "TwentyTwentyOne-December12-solution1"
        , testAction = S12.december12Solution1
        , expectedResult = 4167
        }
    , TestCase
        { testName = "TwentyTwentyOne-December12-solution2"
        , testAction = S12.december12Solution2
        , expectedResult = 98441
        }
    , TestCase
        { testName = "TwentyTwentyOne-December13-solution1"
        , testAction = S13.december13Solution1
        , expectedResult = 942
        }
    , TestCase
        { testName = "TwentyTwentyOne-December13-solution2"
        , testAction = S13.december13Solution2
        , expectedResult = "..##.####..##..#..#..##..###..###..###.  \n...#....#.#..#.#..#.#..#.#..#.#..#.#..# \n...#...#..#....#..#.#..#.#..#.#..#.###.  \n...#..#...#.##.#..#.####.###..###..#..# \n#..#.#....#..#.#..#.#..#.#....#.#..#..# \n.##..####..###..##..#..#.#....#..#.###.\n"
        }
    , TestCase
        { testName = "TwentyTwentyOne-December14-solution1"
        , testAction = S14.december14Solution1
        , expectedResult = 3587
        }
    , TestCase
        { testName = "TwentyTwentyOne-December14-solution2"
        , testAction = S14.december14Solution2
        , expectedResult = 3906445077999
        }
    , TestCase
        { testName = "TwentyTwentyOne-December15-solution1"
        , testAction = S15.december15Solution1
        , expectedResult = 363
        }
    , TestCase
        { testName = "TwentyTwentyOne-December15-solution2"
        , testAction = S15.december15Solution2
        , expectedResult = 2835
        }
    , TestCase
        { testName = "TwentyTwentyOne-December16-solution1"
        , testAction = S16.december16Solution1
        , expectedResult = 979
        }
    , TestCase
        { testName = "TwentyTwentyOne-December16-solution2"
        , testAction = S16.december16Solution2
        , expectedResult = 277110354175
        }
    , TestCase
        { testName = "TwentyTwentyOne-December17-solution1"
        , testAction = S17.december17Solution1
        , expectedResult = 7503
        }
    , TestCase
        { testName = "TwentyTwentyOne-December17-solution2"
        , testAction = S17.december17Solution2
        , expectedResult = 3229
        }
    , TestCase
        { testName = "TwentyTwentyOne-December18-solution1"
        , testAction = S18.december18Solution1
        , expectedResult = 4207
        }
    , TestCase
        { testName = "TwentyTwentyOne-December18-solution2"
        , testAction = S18.december18Solution2
        , expectedResult = 4635
        }
    -- , TestCase
    --     { testName = "TwentyTwentyOne-December19-solution1"
    --     , testAction = S19.december19Solution1
    --     , expectedResult = undefined
    --     }
    -- , TestCase
    --     { testName = "TwentyTwentyOne-December19-solution2"
    --     , testAction = S19.december19Solution2
    --     , expectedResult = undefined
    --     }
    , TestCase
        { testName = "TwentyTwentyOne-December20-solution1"
        , testAction = S20.december20Solution1
        , expectedResult = 4873
        }
    , TestCase
        { testName = "TwentyTwentyOne-December20-solution2"
        , testAction = S20.december20Solution2
        , expectedResult = 16394
        }
    , TestCase
        { testName = "TwentyTwentyOne-December21-solution1"
        , testAction = S21.twentyFirstDecemberSolution1
        , expectedResult = 713328
        }
    , TestCase
        { testName = "TwentyTwentyOne-December21-solution2"
        , testAction = S21.twentyFirstDecemberSolution2
        , expectedResult = 92399285032143
        }
    , TestCase
        { testName = "TwentyTwentyOne-December22-solution1"
        , testAction = S22.twentySecondDecemberSolution1
        , expectedResult = 577205
        }
    , TestCase
        { testName = "TwentyTwentyOne-December22-solution2"
        , testAction = S22.twentySecondDecemberSolution2
        , expectedResult = 1197308251666843
        }
    , TestCase
        { testName = "TwentyTwentyOne-December23-solution1"
        , testAction = return S23.twentyThirdDecemberSolution1
        , expectedResult = 15365
        }
    -- , TestCase
    --     { testName = "TwentyTwentyOne-December23-solution2"
    --     , testAction = S23P2.twentyThirdDecemberSolution2
    --     , expectedResult = undefined
    --     }
    , TestCase
        { testName = "TwentyTwentyOne-December24-solution1"
        , testAction = S24.twentyFourthDecemberSolution1
        , expectedResult = 98998519596997
        }
    , TestCase
        { testName = "TwentyTwentyOne-December24-solution2"
        , testAction = S24.twentyFourthDecemberSolution2
        , expectedResult = 31521119151421
        }
    , TestCase
        { testName = "TwentyTwentyOne-December25-solution1"
        , testAction = S25.twentyFifthDecemberSolution1
        , expectedResult = 353
        }
    ]

