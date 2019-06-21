package setgame.domain

import setgame.domain._
import org.scalatest._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

object GameBoardScalaCheckSpec extends Properties("GameBoard") {

  property("apply always return a board of 12 cards") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard(Deck(), Set.empty[Player]).board.cards.size == 12 }

  property("apply returns always a deck with the total max minus 12") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard(Deck(), Set.empty[Player]).deck.cards.size == 69
    }

  property("apply returns always a deck with the total max minus 12 when input deck is empty") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard(new Deck(Set.empty[Card]), Set.empty[Player]).deck.cards.size == 69
    }

  property("refill always return a board of 3 cards if the input board is empty") =
    forAll(Gen.const("")) { (s : String) => {
      val deck = Deck()
      GameBoard.refill(
        GameState(
          Set.empty[Player],
          deck,
          GameBoard(Set.empty[Card]))
      ).board.cards.size == 3
    }}

  property("refill returns always a deck with the total max minus 3 and a board with plus 3 elements") =
    forAll(Gen.const("")) {
      (s : String) => {
        val deck = Deck()
        val gs = GameBoard(deck, Set.empty[Player])
        val result = GameBoard.refill(gs)
        result.deck.cards.size == 66 && result.board.cards.size == 15
      }
    }

  property("refill returns always a deck with the total max minus 3 if the input deck is empty") =
    forAll(Gen.const("")) { (s : String) =>
      GameBoard.refill(
        GameState(
          Set.empty[Player],
          Deck(Set.empty[Card]),
          GameBoard(Set.empty[Card])
        )
      ).deck.cards.size == 78
    }
}
