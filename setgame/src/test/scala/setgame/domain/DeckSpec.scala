package setgame.domain

import setgame.domain._
import org.scalatest._

class DeckSpec extends WordSpec with Matchers {

  "Deck apply" should {
    "have the expected number of cards" in  {
      Deck().cards.size shouldEqual 81
    }

    "have the expected number of Diamonds" in {
      Deck().cards.filter(_.shape == Diamond).size shouldEqual 27
    }

    "have the expected number of Squiggles" in {
      Deck().cards.filter(_.shape == Squiggle).size shouldEqual 27
    }

    "have the expected number of Ovals" in {
      Deck().cards.filter(_.shape == Oval).size shouldEqual 27
    }

    "have the expected number of Reds" in {
      Deck().cards.filter(_.color == Red).size shouldEqual 27
    }

    "have the expected number of Purples" in {
      Deck().cards.filter(_.color == Purple).size shouldEqual 27
    }

    "have the expected number of Greens" in {
      Deck().cards.filter(_.color == Green).size shouldEqual 27
    }

    "have the expected number of Ones" in {
      Deck().cards.filter(_.number == One).size shouldEqual 27
    }

    "have the expected number of Twos" in {
      Deck().cards.filter(_.number == Two).size shouldEqual 27
    }

    "have the expected number of Threes" in {
      Deck().cards.filter(_.number == Three).size shouldEqual 27
    }

    "have the expected number of Solids" in {
      Deck().cards.filter(_.shading == Solid).size shouldEqual 27
    }

    "have the expected number of Stripes" in {
      Deck().cards.filter(_.shading == Stripe).size shouldEqual 27
    }

    "have the expected number of Outlines" in {
      Deck().cards.filter(_.shading == Outline).size shouldEqual 27
    }
  }
}
