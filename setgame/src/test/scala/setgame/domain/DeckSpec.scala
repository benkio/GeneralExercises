package setgame.domain

import setgame.domain._
import org.scalatest._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object DeckScalaCheckSpec extends Properties("Deck") {

  property("drawCards returns always the expected number of cards") =
    forAll(Gen.chooseNum(0, 81)) { (n : Int) =>
      Deck().flatMap( (d : Deck) =>
        Deck.drawCards(n).runA(d)
      ).unsafeRunSync.size == n
    }

  property("drawCards returns always a deck with the total max minus the drawed ones") =
    forAll(Gen.chooseNum(0, 81)) { (n : Int) =>
      Deck().flatMap( (d : Deck) =>
        Deck.drawCards(n).runS(d)
      ).unsafeRunSync.cards.size == (81-n)
    }

  property("drawCards rebuild a new deck if the deck is empty") =
    forAll(Gen.chooseNum(1, 81)) { (n : Int) =>
      Deck.drawCards(n)
        .runS(new Deck(Set.empty[Card]))
        .unsafeRunSync
        .cards.size == (81-n)
    }
}

class DeckSpec extends WordSpec with Matchers {

  "Deck apply" should {
    "have the expected number of cards" in  {
      Deck().unsafeRunSync.cards.size shouldEqual 81
    }

    "have the expected number of Diamonds" in {
      Deck().unsafeRunSync.cards.filter(_.shape == Diamond).size shouldEqual 27
    }

    "have the expected number of Squiggles" in {
      Deck().unsafeRunSync.cards.filter(_.shape == Squiggle).size shouldEqual 27
    }

    "have the expected number of Ovals" in {
      Deck().unsafeRunSync.cards.filter(_.shape == Oval).size shouldEqual 27
    }

    "have the expected number of Reds" in {
      Deck().unsafeRunSync.cards.filter(_.color == Red).size shouldEqual 27
    }

    "have the expected number of Purples" in {
      Deck().unsafeRunSync.cards.filter(_.color == Purple).size shouldEqual 27
    }

    "have the expected number of Greens" in {
      Deck().unsafeRunSync.cards.filter(_.color == Green).size shouldEqual 27
    }

    "have the expected number of Ones" in {
      Deck().unsafeRunSync.cards.filter(_.number == One).size shouldEqual 27
    }

    "have the expected number of Twos" in {
      Deck().unsafeRunSync.cards.filter(_.number == Two).size shouldEqual 27
    }

    "have the expected number of Threes" in {
      Deck().unsafeRunSync.cards.filter(_.number == Three).size shouldEqual 27
    }

    "have the expected number of Solids" in {
      Deck().unsafeRunSync.cards.filter(_.shading == Solid).size shouldEqual 27
    }

    "have the expected number of Stripes" in {
      Deck().unsafeRunSync.cards.filter(_.shading == Stripe).size shouldEqual 27
    }

    "have the expected number of Outlines" in {
      Deck().unsafeRunSync.cards.filter(_.shading == Outline).size shouldEqual 27
    }
  }

  "Deck drawCards" should {
    "return the same deck and empty set if the requested input is 0" in {
      (for {
        d <- Deck()
        result <- Deck.drawCards(0).run(new Deck(Set.empty[Card]))
      } yield result._2.size == 0 && result._1 == d)
      .unsafeRunSync shouldEqual true
    }
  }
}
