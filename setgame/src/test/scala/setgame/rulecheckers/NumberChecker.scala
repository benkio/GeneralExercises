package setgame.rulecheckers

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import setgame.inputgenerator.InputGenerator
import setgame.domain._
import setgame.rulecheckers.NumberChecker

object NumberCheckerSpec extends Properties("NumberChecker") {

  property("Same number three times") = forAll(InputGenerator.numberGenerator) {
    (s : Number) => NumberChecker.check(s, s, s)
  }

  property("Same number two times but one") =
    forAll(InputGenerator.generateDistinct(InputGenerator.numberGenerator, 2)) {
      (ls : List[Number]) => NumberChecker.check(ls(0), ls(0), ls(1)) == false
    }

  property("All different") =
    forAll(InputGenerator.generateDistinct(InputGenerator.numberGenerator, 3)) {
      (ls : List[Number]) => NumberChecker.check(ls(0), ls(1), ls(2))
    }
}
