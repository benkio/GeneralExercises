package setgame.rulecheckers

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import setgame.inputgenerator.InputGenerator
import setgame.domain._
import setgame.rulecheckers.GameSetChecker
import org.scalatest._

object GameSetCheckerScalaCheckSpec extends Properties("GameSetChecker") {

  property("same card three times") = forAll(InputGenerator.cardGenerator) {
    (c : Card) => GameSetChecker.check(c, c, c)
  }

  property("same card two times and another different card") =
    forAll(InputGenerator.generateDistinct(InputGenerator.cardGenerator, 2)) {
      (lc : List[Card]) => !GameSetChecker.check(lc(0), lc(0), lc(1))
    }
}

class GameSetCheckerSpec extends WordSpec with Matchers {

  import setgame.inputgenerator.FixedInput._

  "GameSetChecker" should {
    "complex valid example" in {
      validDifferentCardInput
        .map(t => GameSetChecker.check(t._1, t._2, t._3))
        .foldLeft(true)(_ && _) shouldEqual true
    }
  }
}
