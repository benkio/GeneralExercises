package setgame.domain

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import setgame.inputgenerator.InputGenerator

object GameSetSpec extends Properties("GameSet") {

  property("apply returns None if the input set has size != from 3") =
    forAll(InputGenerator.cardGenerator, Gen.posNum[Int]) {
      (c : Card, n : Int) => {
        val inputSet : Set[Card] = Set.fill(n % 3)(c)
        GameSet(inputSet) == None
      }
    }

  property("apply the same card three times") =
    forAll(InputGenerator.cardGenerator) {
      (c : Card) => GameSet(Set.fill(3)(c)).isDefined == true
    }

  property("same card two times and another different card") =
    forAll(InputGenerator.generateDistinct(InputGenerator.cardGenerator, 2)) {
      (lc : List[Card]) => GameSet(Set(lc(0), lc(0), lc(1))) == None
    }
}
