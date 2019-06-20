package setgame.rulecheckers

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import setgame.inputgenerator.InputGenerator
import setgame.domain._
import setgame.rulecheckers.ShadingChecker

object ShadingCheckerSpec extends Properties("ShadingChecker") {

  property("Same shading three times") = forAll(InputGenerator.shadingGenerator) {
    (s : Shading) => ShadingChecker.check(s, s, s)
  }

  property("Same shading two times but one") =
    forAll(InputGenerator.generateDistinct(InputGenerator.shadingGenerator, 2)) {
      (ls : List[Shading]) => ShadingChecker.check(ls(0), ls(0), ls(1)) == false
    }

  property("All different") =
    forAll(InputGenerator.generateDistinct(InputGenerator.shadingGenerator, 3)) {
      (ls : List[Shading]) => ShadingChecker.check(ls(0), ls(1), ls(2))
    }
}
