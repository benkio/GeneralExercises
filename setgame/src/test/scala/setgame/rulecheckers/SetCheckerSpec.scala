package setgame.rulecheckers

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import setgame.inputgenerator.InputGenerator
import setgame.domain._
import setgame.rulecheckers.GameSetChecker
import org.scalatest._

object GameSetCheckerSpec extends Properties("GameSetChecker") {

  property("same card three times") = forAll(InputGenerator.cardGenerator) {
    (c : Card) => GameSetChecker.check(c, c, c)
  }

  property("same card two times and another different card") =
    forAll(InputGenerator.generateDistinct(InputGenerator.cardGenerator, 2)) {
      (lc : List[Card]) => !GameSetChecker.check(lc(0), lc(0), lc(1))
    }
}

class GameSetCheckerSpec extends WordSpec with Matchers {

  val validDifferentCardInput : List[(Card, Card, Card)] =
    List(
     (	Card(Oval	, Red	,Two	,Solid),
	Card(Oval	, Purple,One	,Stripe),
	Card(Oval	, Green	,Three	,Outline)),
     (	Card(Squiggle	, Red	,One	,Outline),
	Card(Diamond	, Green	,Two	,Outline),
	Card(Oval	, Purple,Three	,Outline)),
     (	Card(Squiggle	, Red	,Two	,Outline),
	Card(Oval	, Green	,One	,Outline),
	Card(Diamond	, Purple,Three	,Outline)),
     (	Card(Diamond	, Purple,Three	,Solid),
	Card(Oval	, Purple,Two	,Stripe),
	Card(Squiggle	, Purple,One	,Outline)),
     (	Card(Squiggle	, Red	,Two	,Solid),
	Card(Squiggle	, Green	,Three	,Solid),
	Card(Squiggle	, Purple,One	,Solid)),
     (	Card(Diamond	, Red	,Two	,Solid),
	Card(Diamond	, Red	,One	,Stripe),
	Card(Diamond	, Red	,Three	,Outline)))

  "GameSetChecker" should {
    "complex valid example" in {
      validDifferentCardInput
        .map(t => GameSetChecker.check(t._1, t._2, t._3))
        .foldLeft(true)(_ && _)
    }
  }
}
