package setgame.rulecheckers

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import setgame.inputgenerator.InputGenerator
import setgame.domain._
import setgame.rulecheckers.SetChecker

object SetCheckerSpec extends Properties("SetChecker") {

  property("same card three times") = forAll(InputGenerator.cardGenerator) {
    (c : Card) => SetChecker.check(c, c, c)
  }

  property("same card two times and another different card") =
    forAll(InputGenerator.generateDistinct(InputGenerator.cardGenerator, 2)) {
      (lc : List[Card]) => !SetChecker.check(lc(0), lc(0), lc(1))
    }

  // property("All different") =
  //   forAll(InputGenerator.generateDistinct(InputGenerator.cardGenerator, 3)) {
  //     (ls : List[Card]) => SetChecker.check(ls(0), ls(1), ls(2))
  //   }

}
