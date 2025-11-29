module TwentyFifteen.Tests where

import TestCase (TestCase (..))
import qualified TwentyFifteen.December01 as S1
import qualified TwentyFifteen.December02 as S2
import qualified TwentyFifteen.December03 as S3
import qualified TwentyFifteen.December04 as S4
import qualified TwentyFifteen.December05 as S5
import qualified TwentyFifteen.December06 as S6
import qualified TwentyFifteen.December07 as S7
import qualified TwentyFifteen.December08 as S8
import qualified TwentyFifteen.December09 as S9
import qualified TwentyFifteen.December10 as S10
import qualified TwentyFifteen.December11 as S11
import qualified TwentyFifteen.December12 as S12
import qualified TwentyFifteen.December13 as S13
import qualified TwentyFifteen.December14 as S14
import qualified TwentyFifteen.December15 as S15
import qualified TwentyFifteen.December16 as S16
import qualified TwentyFifteen.December17 as S17
import qualified TwentyFifteen.December18 as S18
import qualified TwentyFifteen.December19 as S19
import qualified TwentyFifteen.December20 as S20
import qualified TwentyFifteen.December21 as S21
import qualified TwentyFifteen.December22 as S22
import qualified TwentyFifteen.December23 as S23
import qualified TwentyFifteen.December24 as S24
import qualified TwentyFifteen.December25 as S25

tests :: [TestCase]
tests =
    [ TestCase
        { testName = "TwentyFifteen-December01-solution1"
        , testAction = S1.december01Solution1
        , expectedResult = 232
        }
    , TestCase
        { testName = "TwentyFifteen-December01-solution2"
        , testAction = S1.december01Solution2
        , expectedResult = 1783
        }
    , TestCase
        { testName = "TwentyFifteen-December02-solution1"
        , testAction = S2.december02Solution1
        , expectedResult = 1598415
        }
    , TestCase
        { testName = "TwentyFifteen-December02-solution2"
        , testAction = S2.december02Solution2
        , expectedResult = 3812909
        }
    , TestCase
        { testName = "TwentyFifteen-December03-solution1"
        , testAction = S3.december03Solution1
        , expectedResult = 2572
        }
    , TestCase
        { testName = "TwentyFifteen-December03-solution2"
        , testAction = S3.december03Solution2
        , expectedResult = 2631
        }
    , TestCase
        { testName = "TwentyFifteen-December04-solution1"
        , testAction = S4.december04Solution1
        , expectedResult = 282749
        }
    , TestCase
        { testName = "TwentyFifteen-December04-solution2"
        , testAction = S4.december04Solution2
        , expectedResult = 9962624
        }
    , TestCase
        { testName = "TwentyFifteen-December05-solution1"
        , testAction = S5.december05Solution1
        , expectedResult = 236
        }
    , TestCase
        { testName = "TwentyFifteen-December05-solution2"
        , testAction = S5.december05Solution2
        , expectedResult = 51
        }
    , TestCase
        { testName = "TwentyFifteen-December06-solution1"
        , testAction = S6.december06Solution1
        , expectedResult = 377891
        }
    , TestCase
        { testName = "TwentyFifteen-December06-solution2"
        , testAction = S6.december06Solution2
        , expectedResult = 14110788
        }
    , TestCase
        { testName = "TwentyFifteen-December07-solution1"
        , testAction = S7.december07Solution1
        , expectedResult = 46065
        }
    , TestCase
        { testName = "TwentyFifteen-December07-solution2"
        , testAction = S7.december07Solution2
        , expectedResult = 14134
        }
    , TestCase
        { testName = "TwentyFifteen-December08-solution1"
        , testAction = S8.december08Solution1
        , expectedResult = 1333
        }
    , TestCase
        { testName = "TwentyFifteen-December08-solution2"
        , testAction = S8.december08Solution2
        , expectedResult = 2046
        }
    , TestCase
        { testName = "TwentyFifteen-December09-solution1"
        , testAction = S9.december09Solution1
        , expectedResult = 207
        }
    , TestCase
        { testName = "TwentyFifteen-December09-solution2"
        , testAction = S9.december09Solution2
        , expectedResult = 804
        }
    , TestCase
        { testName = "TwentyFifteen-December10-solution1"
        , testAction = S10.december10Solution1
        , expectedResult = 329356
        }
    , TestCase
        { testName = "TwentyFifteen-December10-solution2"
        , testAction = S10.december10Solution2
        , expectedResult = 4666278
        }
    , TestCase
        { testName = "TwentyFifteen-December11-solution1"
        , testAction = S11.december11Solution1
        , expectedResult = "hepxxyzz"
        }
    , TestCase
        { testName = "TwentyFifteen-December11-solution2"
        , testAction = S11.december11Solution2
        , expectedResult = "heqaabcc"
        }
    , TestCase
        { testName = "TwentyFifteen-December12-solution1"
        , testAction = S12.december12Solution1
        , expectedResult = 111754
        }
    , TestCase
        { testName = "TwentyFifteen-December12-solution2"
        , testAction = S12.december12Solution2
        , expectedResult = 65402
        }
    , TestCase
        { testName = "TwentyFifteen-December13-solution1"
        , testAction = S13.december13Solution1
        , expectedResult = 733
        }
    , TestCase
        { testName = "TwentyFifteen-December13-solution2"
        , testAction = S13.december13Solution2
        , expectedResult = 725
        }
    , TestCase
        { testName = "TwentyFifteen-December14-solution1"
        , testAction = S14.december14Solution1
        , expectedResult = 2660
        }
    , TestCase
        { testName = "TwentyFifteen-December14-solution2"
        , testAction = S14.december14Solution2
        , expectedResult = 1256
        }
    , TestCase
        { testName = "TwentyFifteen-December15-solution1"
        , testAction = S15.december15Solution1
        , expectedResult = 222870
        }
    , TestCase
        { testName = "TwentyFifteen-December15-solution2"
        , testAction = S15.december15Solution2
        , expectedResult = 117936
        }
    , TestCase
        { testName = "TwentyFifteen-December16-solution1"
        , testAction = S16.december16Solution1
        , expectedResult = 40
        }
    , TestCase
        { testName = "TwentyFifteen-December16-solution2"
        , testAction = S16.december16Solution2
        , expectedResult = 241
        }
    , TestCase
        { testName = "TwentyFifteen-December17-solution1"
        , testAction = S17.december17Solution1
        , expectedResult = 4372
        }
    , TestCase
        { testName = "TwentyFifteen-December17-solution2"
        , testAction = S17.december17Solution2
        , expectedResult = 4
        }
    , TestCase
        { testName = "TwentyFifteen-December18-solution1"
        , testAction = S18.december18Solution1
        , expectedResult = 821
        }
    , TestCase
        { testName = "TwentyFifteen-December18-solution2"
        , testAction = S18.december18Solution2
        , expectedResult = 886
        }
    , TestCase
        { testName = "TwentyFifteen-December19-solution1"
        , testAction = S19.december19Solution1
        , expectedResult = 509
        }
    , TestCase
        { testName = "TwentyFifteen-December19-solution2"
        , testAction = S19.december19Solution2
        , expectedResult = 195
        }
    , TestCase
        { testName = "TwentyFifteen-December20-solution1"
        , testAction = S20.december20Solution1
        , expectedResult = 776160
        }
    , TestCase
        { testName = "TwentyFifteen-December20-solution2"
        , testAction = S20.december20Solution2
        , expectedResult = 786240
        }
    , TestCase
        { testName = "TwentyFifteen-December21-solution1"
        , testAction = S21.december21Solution1
        , expectedResult = 111
        }
    , TestCase
        { testName = "TwentyFifteen-December21-solution2"
        , testAction = S21.december21Solution2
        , expectedResult = 188
        }
    , TestCase
        { testName = "TwentyFifteen-December22-solution1"
        , testAction = S22.december22Solution1
        , expectedResult = 1824
        }
    , TestCase
        { testName = "TwentyFifteen-December22-solution2"
        , testAction = S22.december22Solution2
        , expectedResult = 1937
        }
    , TestCase
        { testName = "TwentyFifteen-December23-solution1"
        , testAction = S23.december23Solution1
        , expectedResult = 170
        }
    , TestCase
        { testName = "TwentyFifteen-December23-solution2"
        , testAction = S23.december23Solution2
        , expectedResult = 247
        }
    , TestCase
        { testName = "TwentyFifteen-December24-solution1"
        , testAction = S24.december24Solution1
        , expectedResult = 10439961859
        }
    , TestCase
        { testName = "TwentyFifteen-December24-solution2"
        , testAction = S24.december24Solution2
        , expectedResult = 72050269
        }
    , TestCase
        { testName = "TwentyFifteen-December25-solution1"
        , testAction = S25.december25Solution1
        , expectedResult = 9132360
        }
    ]
