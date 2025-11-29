module TwentyTwentyFour.Tests where

import TestCase (TestCase (..))
import qualified TwentyTwentyFour.December01 as S1
import qualified TwentyTwentyFour.December02 as S2
import qualified TwentyTwentyFour.December03 as S3
import qualified TwentyTwentyFour.December04 as S4
import qualified TwentyTwentyFour.December05 as S5
import qualified TwentyTwentyFour.December06 as S6
import qualified TwentyTwentyFour.December07 as S7
import qualified TwentyTwentyFour.December08 as S8
import qualified TwentyTwentyFour.December09 as S9
import qualified TwentyTwentyFour.December10 as S10
import qualified TwentyTwentyFour.December11 as S11
import qualified TwentyTwentyFour.December12 as S12
import qualified TwentyTwentyFour.December13 as S13
import qualified TwentyTwentyFour.December14 as S14
import qualified TwentyTwentyFour.December15a as S15
import qualified TwentyTwentyFour.December15b as S15
import qualified TwentyTwentyFour.December16 as S16
import qualified TwentyTwentyFour.December17 as S17
import qualified TwentyTwentyFour.December18 as S18
import qualified TwentyTwentyFour.December19 as S19
import qualified TwentyTwentyFour.December20 as S20
import qualified TwentyTwentyFour.December21 as S21
import qualified TwentyTwentyFour.December22 as S22
import qualified TwentyTwentyFour.December23 as S23
import qualified TwentyTwentyFour.December24 as S24
import qualified TwentyTwentyFour.December25 as S25

tests :: [TestCase]
tests =
    [ TestCase
        { testName = "TwentyTwentyFour-December01-solution1"
        , testAction = S1.december01Solution1
        , expectedResult = 2378066
        }
    , TestCase
        { testName = "TwentyTwentyFour-December01-solution2"
        , testAction = S1.december01Solution2
        , expectedResult = 18934359
        }
    , TestCase
        { testName = "TwentyTwentyFour-December02-solution1"
        , testAction = S2.december02Solution1
        , expectedResult = 510
        }
    , TestCase
        { testName = "TwentyTwentyFour-December02-solution2"
        , testAction = S2.december02Solution2
        , expectedResult = 553
        }
    , TestCase
        { testName = "TwentyTwentyFour-December03-solution1"
        , testAction = S3.december03Solution1
        , expectedResult = 189527826
        }
    , TestCase
        { testName = "TwentyTwentyFour-December03-solution2"
        , testAction = S3.december03Solution2
        , expectedResult = 63013756
        }
    , TestCase
        { testName = "TwentyTwentyFour-December04-solution1"
        , testAction = S4.december04Solution1
        , expectedResult = 2603
        }
    , TestCase
        { testName = "TwentyTwentyFour-December04-solution2"
        , testAction = S4.december04Solution2
        , expectedResult = 1965
        }
    , TestCase
        { testName = "TwentyTwentyFour-December05-solution1"
        , testAction = S5.december05Solution1
        , expectedResult = 6242
        }
    , TestCase
        { testName = "TwentyTwentyFour-December05-solution2"
        , testAction = S5.december05Solution2
        , expectedResult = 5169
        }
    , TestCase
        { testName = "TwentyTwentyFour-December06-solution1"
        , testAction = S6.december06Solution1
        , expectedResult = 5444
        }
    , TestCase
        { testName = "TwentyTwentyFour-December06-solution2"
        , testAction = S6.december06Solution2
        , expectedResult = 1946
        }
    , TestCase
        { testName = "TwentyTwentyFour-December07-solution1"
        , testAction = S7.december07Solution1
        , expectedResult = 1298300076754
        }
    , TestCase
        { testName = "TwentyTwentyFour-December07-solution2"
        , testAction = S7.december07Solution2
        , expectedResult = 248427118972289
        }
    , TestCase
        { testName = "TwentyTwentyFour-December08-solution1"
        , testAction = S8.december08Solution1
        , expectedResult = 289
        }
    , TestCase
        { testName = "TwentyTwentyFour-December08-solution2"
        , testAction = S8.december08Solution2
        , expectedResult = 1030
        }
    , TestCase
        { testName = "TwentyTwentyFour-December09-solution1"
        , testAction = S9.december09Solution1
        , expectedResult = 6359213660505
        }
    , TestCase
        { testName = "TwentyTwentyFour-December09-solution2"
        , testAction = S9.december09Solution2
        , expectedResult = 6381624803796
        }
    , TestCase
        { testName = "TwentyTwentyFour-December10-solution1"
        , testAction = S10.december10Solution1
        , expectedResult = 646
        }
    , TestCase
        { testName = "TwentyTwentyFour-December10-solution2"
        , testAction = S10.december10Solution2
        , expectedResult = 1494
        }
    , TestCase
        { testName = "TwentyTwentyFour-December11-solution1"
        , testAction = S11.december11Solution1
        , expectedResult = 209412
        }
    , TestCase
        { testName = "TwentyTwentyFour-December11-solution2"
        , testAction = S11.december11Solution2
        , expectedResult = 248967696501656
        }
    , TestCase
        { testName = "TwentyTwentyFour-December12-solution1"
        , testAction = S12.december12Solution1
        , expectedResult = 1319878
        }
    , TestCase
        { testName = "TwentyTwentyFour-December12-solution2"
        , testAction = S12.december12Solution2
        , expectedResult = 784982
        }
    , TestCase
        { testName = "TwentyTwentyFour-December13-solution1"
        , testAction = S13.december13Solution1
        , expectedResult = 35729
        }
    , TestCase
        { testName = "TwentyTwentyFour-December13-solution2"
        , testAction = S13.december13Solution2
        , expectedResult = 88584689879723
        }
    , TestCase
        { testName = "TwentyTwentyFour-December14-solution1"
        , testAction = S14.december14Solution1
        , expectedResult = 225521010
        }
    , TestCase
        { testName = "TwentyTwentyFour-December14-solution2"
        , testAction = S14.december14Solution2
        , expectedResult = 7774
        }
    , TestCase
        { testName = "TwentyTwentyFour-December15a-solution1"
        , testAction = S15.december15Solution1
        , expectedResult = 1441031
        }
    , TestCase
        { testName = "TwentyTwentyFour-December15b-solution2"
        , testAction = S15.december15Solution2
        , expectedResult = 1425169
        }
    , TestCase
        { testName = "TwentyTwentyFour-December16-solution1"
        , testAction = S16.december16Solution1
        , expectedResult = 90440
        }
    , TestCase
        { testName = "TwentyTwentyFour-December16-solution2"
        , testAction = S16.december16Solution2
        , expectedResult = 479
        }
    , TestCase
        { testName = "TwentyTwentyFour-December17-solution1"
        , testAction = S17.december17Solution1
        , expectedResult = "4,3,7,1,5,3,0,5,4"
        }
    , TestCase
        { testName = "TwentyTwentyFour-December17-solution2"
        , testAction = S17.december17Solution2
        , expectedResult = 190384615275535
        }
    , TestCase
        { testName = "TwentyTwentyFour-December18-solution1"
        , testAction = S18.december18Solution1
        , expectedResult = 268
        }
    , TestCase
        { testName = "TwentyTwentyFour-December18-solution2"
        , testAction = S18.december18Solution2
        , expectedResult = "64,11"
        }
    , TestCase
        { testName = "TwentyTwentyFour-December19-solution1"
        , testAction = S19.december19Solution1
        , expectedResult = 360
        }
    , TestCase
        { testName = "TwentyTwentyFour-December19-solution2"
        , testAction = S19.december19Solution2
        , expectedResult = 577474410989846
        }
    , TestCase
        { testName = "TwentyTwentyFour-December20-solution1"
        , testAction = S20.december20Solution1
        , expectedResult = 1317
        }
    , TestCase
        { testName = "TwentyTwentyFour-December20-solution2"
        , testAction = S20.december20Solution2
        , expectedResult = 982474
        }
    , TestCase
        { testName = "TwentyTwentyFour-December21-solution1"
        , testAction = S21.december21Solution1
        , expectedResult = 152942
        }
    , TestCase
        { testName = "TwentyTwentyFour-December21-solution2"
        , testAction = S21.december21Solution2
        , expectedResult = 189235298434780
        }
    , TestCase
        { testName = "TwentyTwentyFour-December22-solution1"
        , testAction = S22.december22Solution1
        , expectedResult = 13584398738
        }
    , TestCase
        { testName = "TwentyTwentyFour-December22-solution2"
        , testAction = S22.december22Solution2
        , expectedResult = 1612
        }
    , TestCase
        { testName = "TwentyTwentyFour-December23-solution1"
        , testAction = S23.december23Solution1
        , expectedResult = 1215
        }
    , TestCase
        { testName = "TwentyTwentyFour-December23-solution2"
        , testAction = S23.december23Solution2
        , expectedResult = "bm,by,dv,ep,ia,ja,jb,ks,lv,ol,oy,uz,yt"
        }
    , TestCase
        { testName = "TwentyTwentyFour-December24-solution1"
        , testAction = S24.december24Solution1
        , expectedResult = 50411513338638
        }
    , TestCase
        { testName = "TwentyTwentyFour-December24-solution2"
        , testAction = S24.december24Solution2
        , expectedResult = "gfv,hcm,kfs,tqm,vwr,z06,z11,z16"
        }
    , TestCase
        { testName = "TwentyTwentyFour-December25-solution1"
        , testAction = S25.december25Solution1
        , expectedResult = 2835
        }
    ]
