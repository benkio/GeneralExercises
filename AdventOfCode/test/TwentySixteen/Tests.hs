module TwentySixteen.Tests where

import TestCase (TestCase (..))
import qualified TwentySixteen.December01 as S1
import qualified TwentySixteen.December02 as S2
import qualified TwentySixteen.December03 as S3
import qualified TwentySixteen.December04 as S4
import qualified TwentySixteen.December05 as S5
import qualified TwentySixteen.December06 as S6
import qualified TwentySixteen.December07 as S7
import qualified TwentySixteen.December08 as S8
import qualified TwentySixteen.December09 as S9
import qualified TwentySixteen.December10 as S10
import qualified TwentySixteen.December12 as S12
import qualified TwentySixteen.December13 as S13
import qualified TwentySixteen.December14 as S14
import qualified TwentySixteen.December15 as S15
import qualified TwentySixteen.December16 as S16
import qualified TwentySixteen.December17 as S17
import qualified TwentySixteen.December18 as S18
import qualified TwentySixteen.December19 as S19
import qualified TwentySixteen.December20 as S20
import qualified TwentySixteen.December21 as S21
import qualified TwentySixteen.December22 as S22
import qualified TwentySixteen.December23 as S23
import qualified TwentySixteen.December24 as S24
import qualified TwentySixteen.December25 as S25

tests :: [TestCase]
tests =
    [       TestCase
        { testName = "TwentySixteen-December01-solution1"
        , testAction = S1.december01Solution1
        , expectedResult = 299
        }
    , TestCase
        { testName = "TwentySixteen-December01-solution2"
        , testAction = S1.december01Solution2
        , expectedResult = 181
        }
    , TestCase
        { testName = "TwentySixteen-December02-solution1"
        , testAction = S2.december02Solution1
        , expectedResult = "36629"
        }
    , TestCase
        { testName = "TwentySixteen-December02-solution2"
        , testAction = S2.december02Solution2
        , expectedResult = "99C3D"
        }
    , TestCase
        { testName = "TwentySixteen-December03-solution1"
        , testAction = S3.december03Solution1
        , expectedResult = 917
        }
    , TestCase
        { testName = "TwentySixteen-December03-solution2"
        , testAction = S3.december03Solution2
        , expectedResult = 1649
        }
    , TestCase
        { testName = "TwentySixteen-December04-solution1"
        , testAction = S4.december04Solution1
        , expectedResult = 278221
        }
    , TestCase
        { testName = "TwentySixteen-December04-solution2"
        , testAction = S4.december04Solution2
        , expectedResult = 267
        }
    , TestCase
        { testName = "TwentySixteen-December05-solution1"
        , testAction = S5.december05Solution1
        , expectedResult = "c6697b55"
        }
    , TestCase
        { testName = "TwentySixteen-December05-solution2"
        , testAction = S5.december05Solution2
        , expectedResult = "8c35d1ab"
        }
    , TestCase
        { testName = "TwentySixteen-December06-solution1"
        , testAction = S6.december06Solution1
        , expectedResult = "agmwzecr"
        }
    , TestCase
        { testName = "TwentySixteen-December06-solution2"
        , testAction = S6.december06Solution2
        , expectedResult = "owlaxqvq"
        }
    , TestCase
        { testName = "TwentySixteen-December07-solution1"
        , testAction = S7.december07Solution1
        , expectedResult = 118
        }
    , TestCase
        { testName = "TwentySixteen-December07-solution2"
        , testAction = S7.december07Solution2
        , expectedResult = 260
        }
    , TestCase
        { testName = "TwentySixteen-December08-solution1"
        , testAction = S8.december08Solution1
        , expectedResult = 116
        }
    , TestCase
        { testName = "TwentySixteen-December08-solution2"
        , testAction = S8.december08Solution2
        , expectedResult = "#..#.###...##....##.####.#....###...##..####.####.\n#..#.#..#.#..#....#.#....#....#..#.#..#.#.......#.\n#..#.#..#.#..#....#.###..#....###..#....###....#..\n#..#.###..#..#....#.#....#....#..#.#....#.....#...\n#..#.#....#..#.#..#.#....#....#..#.#..#.#....#....\n.##..#.....##...##..#....####.###...##..####.####.\n"
        }
    , TestCase
        { testName = "TwentySixteen-December09-solution1"
        , testAction = S9.december09Solution1
        , expectedResult = 138735
        }
    , TestCase
        { testName = "TwentySixteen-December09-solution2"
        , testAction = S9.december09Solution2
        , expectedResult = 11125026826
        }
    , TestCase
        { testName = "TwentySixteen-December10-solution1"
        , testAction = S10.december10Solution1
        , expectedResult = 147
        }
    , TestCase
        { testName = "TwentySixteen-December10-solution2"
        , testAction = S10.december10Solution2
        , expectedResult = 55637
        }
    , TestCase
        { testName = "TwentySixteen-December12-solution1"
        , testAction = S12.december12Solution1
        , expectedResult = 318003
        }
    , TestCase
        { testName = "TwentySixteen-December12-solution2"
        , testAction = S12.december12Solution2
        , expectedResult = 9227657
        }
    , TestCase
        { testName = "TwentySixteen-December13-solution1"
        , testAction = S13.december13Solution1
        , expectedResult = 86
        }
    , TestCase
        { testName = "TwentySixteen-December13-solution2"
        , testAction = S13.december13Solution2
        , expectedResult = 127
        }
    , TestCase
        { testName = "TwentySixteen-December14-solution1"
        , testAction = S14.december14Solution1
        , expectedResult = 23769
        }
    , TestCase
        { testName = "TwentySixteen-December14-solution2"
        , testAction = S14.december14Solution2
        , expectedResult = 20606
        }
    , TestCase
        { testName = "TwentySixteen-December15-solution1"
        , testAction = S15.december15Solution1
        , expectedResult = 317371
        }
    , TestCase
        { testName = "TwentySixteen-December15-solution2"
        , testAction = S15.december15Solution2
        , expectedResult = 2080951
        }
    , TestCase
        { testName = "TwentySixteen-December16-solution1"
        , testAction = S16.december16Solution1
        , expectedResult = "10100011010101011"
        }
    , TestCase
        { testName = "TwentySixteen-December16-solution2"
        , testAction = S16.december16Solution2
        , expectedResult = "01010001101011001"
        }
    , TestCase
        { testName = "TwentySixteen-December17-solution1"
        , testAction = S17.december17Solution1
        , expectedResult = "RDRDUDLRDR"
        }
    , TestCase
        { testName = "TwentySixteen-December17-solution2"
        , testAction = S17.december17Solution2
        , expectedResult = 386
        }
    , TestCase
        { testName = "TwentySixteen-December18-solution1"
        , testAction = S18.december18Solution1
        , expectedResult = 1913
        }
    , TestCase
        { testName = "TwentySixteen-December18-solution2"
        , testAction = S18.december18Solution2
        , expectedResult = 19993564
        }
    , TestCase
        { testName = "TwentySixteen-December19-solution1"
        , testAction = S19.december19Solution1
        , expectedResult = 1834903
        }
    , TestCase
        { testName = "TwentySixteen-December19-solution2"
        , testAction = S19.december19Solution2
        , expectedResult = 1420280
        }
    , TestCase
        { testName = "TwentySixteen-December20-solution1"
        , testAction = S20.december20Solution1
        , expectedResult = 14975795
        }
    , TestCase
        { testName = "TwentySixteen-December20-solution2"
        , testAction = S20.december20Solution2
        , expectedResult = 101
        }
    , TestCase
        { testName = "TwentySixteen-December21-solution1"
        , testAction = S21.december21Solution1
        , expectedResult = "gbhafcde"
        }
    , TestCase
        { testName = "TwentySixteen-December21-solution2"
        , testAction = S21.december21Solution2
        , expectedResult = "bcfaegdh"
        }
    , TestCase
        { testName = "TwentySixteen-December22-solution1"
        , testAction = S22.december22Solution1
        , expectedResult = 910
        }
    , TestCase
        { testName = "TwentySixteen-December22-solution2"
        , testAction = S22.december22Solution2
        , expectedResult = 222
        }
    , TestCase
        { testName = "TwentySixteen-December23-solution1"
        , testAction = S23.december23Solution1
        , expectedResult = 12480
        }
    , TestCase
        { testName = "TwentySixteen-December23-solution2"
        , testAction = S23.december23Solution2
        , expectedResult = 479009040
        }
    , TestCase
        { testName = "TwentySixteen-December24-solution1"
        , testAction = S24.december24Solution1
        , expectedResult = 442
        }
    , TestCase
        { testName = "TwentySixteen-December24-solution2"
        , testAction = S24.december24Solution2
        , expectedResult = 660
        }
    , TestCase
        { testName = "TwentySixteen-December25-solution1"
        , testAction = S25.twentyfifthDecemberSolution
        , expectedResult = 175
        }
    ]
