package setgame.domain

import setgame.domain._
import org.scalatest._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object GameBoardScalaCheckSpec extends Properties("GameBoard") {

  property("drawCards returns always the expected number of cards") =
    forAll(Gen.chooseNum(0, 12)) { (n : Int) => (for {
      d <- Deck()
      deckNBoard <- GameBoard.build.run(d)
      cards <- GameBoard.drawCards(deckNBoard._1, n).runA(deckNBoard._2)
    } yield cards).unsafeRunSync.size == n
    }

  property("drawCards returns always a deck with the total max minus the drawed ones") =
    forAll(Gen.chooseNum(0, 12)) { (n : Int) => (for {
      d <- Deck()
      deckNBoard <- GameBoard.build.run(d)
      resultDeck <- GameBoard.drawCards(deckNBoard._1, n).runS(deckNBoard._2)
    } yield resultDeck.cards).unsafeRunSync.size == (12-n)
    }

  property("drawCards rebuild a new deck if the deck is empty") =
    forAll(Gen.chooseNum(1, 12)) { (n : Int) => (for {
      d <- Deck()
      deckNBoard <- GameBoard.build.run(d)
      resultDeck <- GameBoard.drawCards(new Deck(Set.empty[Card]), n).runS(deckNBoard._2)
    } yield resultDeck.cards).unsafeRunSync.size == (12-n)
    }

  property("build always return a board of 12 cards") =
    forAll(Gen.const("")) { (s : String) => (for {
      d <- Deck()
      board <- GameBoard.build.runA(d)
    } yield board)
      .unsafeRunSync.cards.size == 12
    }

  property("build returns always a deck with the total max minus 12") =
    forAll(Gen.const("")) { (s : String) => (for {
      d <- Deck()
      deck <- GameBoard.build.runS(d)
    } yield deck)
      .unsafeRunSync.cards.size  == 69
    }

  property("build returns always a deck with the total max minus 12 when input deck is empty") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard.build.runS(new Deck(Set.empty[Card]))
        .unsafeRunSync
        .cards.size == 69
    }

  property("refill always return a board of 3 cards if the input board is empty") =
    forAll(Gen.const("")) { (s : String) => (for {
      deck <- Deck()
      board <- GameBoard.refill(
        GameBoard(Set.empty[Card])
      ).runA(deck)
    } yield board).unsafeRunSync.cards.size == 3
    }

  property("refill returns always a deck with the total max minus 3 and a board with plus 3 elements") =
    forAll(Gen.const("")) {
      (s : String) => (for {
         gs <- GameBoard.build
        result <- GameBoard.refill(gs)
      } yield result)
        .runF
        .flatMap(f => Deck().flatMap(f(_)))
        .map { case (deck : Deck, board : GameBoard) =>
          deck.cards.size == 66 && board.cards.size == 15
        }
      .unsafeRunSync
    }

  property("refill returns always a deck with the total max minus 3 if the input deck is empty") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard.refill(
          GameBoard(Set.empty[Card])
      ).runS(Deck(Set.empty[Card]))
        .unsafeRunSync.cards.size == 78
    }
}
