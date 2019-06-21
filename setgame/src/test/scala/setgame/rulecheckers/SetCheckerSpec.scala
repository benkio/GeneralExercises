package setgame.rulecheckers

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import setgame.inputgenerator.InputGenerator
import setgame.domain._
import setgame.rulecheckers.SetChecker
import org.scalatest._

object SetCheckerSpec extends Properties("SetChecker") {

  property("same card three times") = forAll(InputGenerator.cardGenerator) {
    (c : Card) => SetChecker.check(c, c, c)
  }

  property("same card two times and another different card") =
    forAll(InputGenerator.generateDistinct(InputGenerator.cardGenerator, 2)) {
      (lc : List[Card]) => !SetChecker.check(lc(0), lc(0), lc(1))
    }
}

class SetCheckerSpec extends WordSpec with Matchers {

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

  "SetChecker" should {
    "complex valid example" in {
      validDifferentCardInput
        .map(t => SetChecker.check(t._1, t._2, t._3))
        .foldLeft(true)(_ && _)
    }
  }
}
