package setgame.domain

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.scalatest._
import setgame.inputgenerator.InputGenerator

object GameSetScalaCheckSpec extends Properties("GameSet") {

  property("apply returns None if the input set has size != from 3") =
    forAll(InputGenerator.cardGenerator, Gen.posNum[Int]) {
      (c : Card, n : Int) => {
        val inputSet : Set[Card] = Set.fill(n % 3)(c)
        GameSet(inputSet) == None
      }
    }

  property("same card two times and another different card") =
    forAll(InputGenerator.generateDistinct(InputGenerator.cardGenerator, 2)) {
      (lc : List[Card]) => GameSet(Set(lc(0), lc(0), lc(1))) == None
    }
}

class GameSetSpec extends WordSpec with Matchers {

  import setgame.inputgenerator.FixedInput._

  "GameSet" should {
    "return Some if the input is valid" in {
      validDifferentCardInput
        .map { case (c1, c2, c3) => GameSet(Set(c1, c2, c3)) }
        .forall(_.isDefined) shouldEqual true
    }
  }

}
