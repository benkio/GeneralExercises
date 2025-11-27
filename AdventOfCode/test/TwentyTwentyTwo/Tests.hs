module TwentyTwentyTwo.Tests where

import TestCase (TestCase (..))
import qualified TwentyTwentyTwo.December01 as S1
import qualified TwentyTwentyTwo.December02 as S2
import qualified TwentyTwentyTwo.December03 as S3
import qualified TwentyTwentyTwo.December04 as S4
import qualified TwentyTwentyTwo.December05 as S5
import qualified TwentyTwentyTwo.December06 as S6
import qualified TwentyTwentyTwo.December07 as S7
import qualified TwentyTwentyTwo.December08 as S8
import qualified TwentyTwentyTwo.December09 as S9
import qualified TwentyTwentyTwo.December10 as S10
import qualified TwentyTwentyTwo.December11 as S11
import qualified TwentyTwentyTwo.December12 as S12
import qualified TwentyTwentyTwo.December13 as S13
import qualified TwentyTwentyTwo.December14 as S14
import qualified TwentyTwentyTwo.December15 as S15
import qualified TwentyTwentyTwo.December16 as S16
import qualified TwentyTwentyTwo.December17 as S17
import qualified TwentyTwentyTwo.December18 as S18
import qualified TwentyTwentyTwo.December19 as S19
import qualified TwentyTwentyTwo.December20 as S20
import qualified TwentyTwentyTwo.December21 as S21
import qualified TwentyTwentyTwo.December22 as S22
import qualified TwentyTwentyTwo.December23 as S23
import qualified TwentyTwentyTwo.December24 as S24
import qualified TwentyTwentyTwo.December25 as S25

tests :: [TestCase]
tests =
    [       TestCase
        { testName = "TwentyTwentyTwo-December01-solution1"
        , testAction = S1.december01Solution1
        , expectedResult = 74198
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December01-solution2"
        , testAction = S1.december01Solution2
        , expectedResult = 209914
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December02-solution1"
        , testAction = S2.december02Solution1
        , expectedResult = 17189
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December02-solution2"
        , testAction = S2.december02Solution2
        , expectedResult = 13490
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December03-solution1"
        , testAction = S3.december03Solution1
        , expectedResult = 8123
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December03-solution2"
        , testAction = S3.december03Solution2
        , expectedResult = 2620
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December04-solution1"
        , testAction = S4.december04Solution1
        , expectedResult = 462
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December04-solution2"
        , testAction = S4.december04Solution2
        , expectedResult = 835
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December05-solution1"
        , testAction = S5.december05Solution1
        , expectedResult = "TLNGFGMFN"
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December05-solution2"
        , testAction = S5.december05Solution2
        , expectedResult = "FGLQJCMBD"
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December06-solution1"
        , testAction = S6.december06Solution1
        , expectedResult = 1850
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December06-solution2"
        , testAction = S6.december06Solution2
        , expectedResult = 2823
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December07-solution1"
        , testAction = S7.december07Solution1
        , expectedResult = 1667443
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December07-solution2"
        , testAction = S7.december07Solution2
        , expectedResult = 8998590
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December08-solution1"
        , testAction = S8.december08Solution1
        , expectedResult = 1825
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December08-solution2"
        , testAction = S8.december08Solution2
        , expectedResult = 235200
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December09-solution1"
        , testAction = S9.december09Solution1
        , expectedResult = 6486
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December09-solution2"
        , testAction = S9.december09Solution2
        , expectedResult = 2678
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December10-solution1"
        , testAction = S10.december10Solution1
        , expectedResult = 14220
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December10-solution2"
        , testAction = S10.december10Solution2
        , expectedResult = "\n####.###...##..###..#....####.####.#..#.\n...#.#..#.#..#.#..#.#....#.......#.#..#.\n..#..#..#.#..#.#..#.#....###....#..#..#.\n.#...###..####.###..#....#.....#...#..#.\n#....#.#..#..#.#.#..#....#....#....#..#.\n####.#..#.#..#.#..#.####.#....####..##.."
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December11-solution1"
        , testAction = S11.december11Solution1
        , expectedResult = 67830
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December11-solution2"
        , testAction = S11.december11Solution2
        , expectedResult = 15305381442
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December12-solution1"
        , testAction = S12.december12Solution1
        , expectedResult = 520
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December12-solution2"
        , testAction = S12.december12Solution2
        , expectedResult = 508
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December13-solution1"
        , testAction = S13.december13Solution1
        , expectedResult = 6072
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December13-solution2"
        , testAction = S13.december13Solution2
        , expectedResult = 22184
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December14-solution1"
        , testAction = S14.december14Solution1
        , expectedResult = 757
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December14-solution2"
        , testAction = S14.december14Solution2
        , expectedResult = 24943
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December15-solution1"
        , testAction = S15.december15Solution1
        , expectedResult = 5832528
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December15-solution2"
        , testAction = S15.december15Solution2
        , expectedResult = 13360899249595
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December16-solution1"
        , testAction = S16.december16Solution1
        , expectedResult = 1947
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December16-solution2"
        , testAction = S16.december16Solution2
        , expectedResult = 2556
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December17-solution1"
        , testAction = S17.december17Solution1
        , expectedResult = 3130
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December17-solution2"
        , testAction = S17.december17Solution2
        , expectedResult = 1556521739139
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December18-solution1"
        , testAction = S18.december18Solution1
        , expectedResult = 4320
        }
            , TestCase
        { testName = "TwentyTwentyTwo-December18-solution2"
        , testAction = S18.december18Solution2
        , expectedResult = 2456
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December19-solution1"
        , testAction = S19.december19Solution1
        , expectedResult = 1199
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December19-solution2"
        , testAction = S19.december19Solution2
        , expectedResult = 3510
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December20-solution1"
        , testAction = S20.december20Solution1
        , expectedResult = 3700
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December20-solution2"
        , testAction = S20.december20Solution2
        , expectedResult = 10626948369382
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December21-solution1"
        , testAction = S21.twentyFirstDecemberSolution1
        , expectedResult = 54703080378102
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December21-solution2"
        , testAction = S21.twentyFirstDecemberSolution2
        , expectedResult = 3952673930912
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December22-solution1"
        , testAction = S22.twentySecondDecemberSolution1
        , expectedResult = 126350
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December22-solution2"
        , testAction = S22.twentySecondDecemberSolution2
        , expectedResult = 129339
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December23-solution1"
        , testAction = S23.twentyThirdDecemberSolution1
        , expectedResult = 3757
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December23-solution2"
        , testAction = S23.twentyThirdDecemberSolution2
        , expectedResult = 918
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December24-solution1"
        , testAction = S24.twentyFourthDecemberSolution1
        , expectedResult = 292
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December24-solution2"
        , testAction = S24.twentyFourthDecemberSolution2
        , expectedResult = 816
        }
    , TestCase
        { testName = "TwentyTwentyTwo-December25-solution1"
        , testAction = S25.twentyFifthDecemberSolution1
        , expectedResult = "122-2=200-0111--=200"
        }
    ]
