package setgame.domain

import scala.util.Random

//No private constructor for testing purposes
case class Deck(cards: Set[Card])
case class GameBoard(cards: Set[Card])

object Deck {

  def apply() : Deck = new Deck(
    Random.shuffle(
      for {
        sp <- Shape.values
        c  <- Color.values
        n  <- Number.values
        sd <- Shading.values
      } yield Card(sp, c, n, sd)).toSet
  )

  def drawCards(n : Int, deck : Deck) : (Set[Card], Deck) =
    if (deck.cards.size >= n)
      (deck.cards.take(n), new Deck(deck.cards.drop(n)))
    else drawCards(n, Deck())
}

object GameBoard {

  def apply(deck : Deck) : (GameBoard, Deck) = {
    val (cards, newDeck) = Deck.drawCards(12, deck)
    (new GameBoard(cards), newDeck)
  }

  /**
    * Called to refill the board after a player successful set or after a round without sets
    */
  def refill(board : GameBoard, deck : Deck) : (GameBoard, Deck) = {
    val (cards, newDeck) = Deck.drawCards(3, deck)
    (new GameBoard(board.cards ++ cards), newDeck)
  }

}
