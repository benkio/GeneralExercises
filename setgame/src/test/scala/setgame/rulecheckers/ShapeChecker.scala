package setgame.rulecheckers

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import setgame.inputgenerator.InputGenerator
import setgame.domain._
import setgame.rulecheckers.ShapeChecker

object ShapeCheckerSpec extends Properties("ShapeChecker") {

  property("Same shape three times") = forAll(InputGenerator.shapeGenerator) {
    (s : Shape) => ShapeChecker.check(s, s, s)
  }

  property("Same shape two times but one") =
    forAll(InputGenerator.generateDistinct(InputGenerator.shapeGenerator, 2)) {
      (ls : List[Shape]) => ShapeChecker.check(ls(0), ls(0), ls(1)) == false
    }

  property("All different") =
    forAll(InputGenerator.generateDistinct(InputGenerator.shapeGenerator, 3)) {
      (ls : List[Shape]) => ShapeChecker.check(ls(0), ls(1), ls(2))
    }
}
