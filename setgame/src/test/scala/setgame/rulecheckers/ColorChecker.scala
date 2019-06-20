package setgame.rulecheckers

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import setgame.inputgenerator.InputGenerator
import setgame.domain._
import setgame.rulecheckers.ColorChecker

object ColorCheckerSpec extends Properties("ColorChecker") {

  property("Same color three times") = forAll(InputGenerator.colorGenerator) {
    (s : Color) => ColorChecker.check(s, s, s)
  }

  property("Same color two times but one") =
    forAll(InputGenerator.generateDistinct(InputGenerator.colorGenerator, 2)) {
      (ls : List[Color]) => ColorChecker.check(ls(0), ls(0), ls(1)) == false
    }

  property("All different") =
    forAll(InputGenerator.generateDistinct(InputGenerator.colorGenerator, 3)) {
      (ls : List[Color]) => ColorChecker.check(ls(0), ls(1), ls(2))
    }
}
