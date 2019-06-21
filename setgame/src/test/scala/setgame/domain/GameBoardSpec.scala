package setgame.domain

import setgame.domain._
import org.scalatest._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object GameBoardScalaCheckSpec extends Properties("GameBoard") {

  property("apply always return a board of 12 cards") =
    forAll(Gen.const("")) { (s : String) => GameBoard(Deck())._1.cards.size == 12 }

  property("apply returns always a deck with the total max minus 12") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard(Deck())._2.cards.size == 69
    }

  property("apply returns always a deck with the total max minus 12 when input deck is empty") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard(new Deck(Set.empty[Card]))._2.cards.size == 69
    }

  property("refill always return a board of 3 cards if the input board is empty") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard.refill(new GameBoard(Set.empty[Card]) , Deck())._1.cards.size == 3 }

  property("refill returns always a deck with the total max minus 3 and a board with plus 3 elements") =
    forAll(Gen.const("")) {
      (s : String) => {
        val deck = Deck()
        val (board, newDeck) = GameBoard(deck)
        val result = GameBoard.refill(board, newDeck)
        result._2.cards.size == 66 && result._1.cards.size == 15
      }
    }

  property("refill returns always a deck with the total max minus 3 if the input deck is empty") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard.refill(GameBoard(Deck())._1, new Deck(Set.empty[Card]))._2.cards.size == 78
    }
}
