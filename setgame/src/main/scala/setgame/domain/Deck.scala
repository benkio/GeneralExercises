package setgame.domain

import scala.util.Random
import cats.effect.IO
import cats.data._

//No private constructor for testing purposes
case class Deck(cards: Set[Card])
case class GameBoard(cards: Set[Card])

object Deck {

  def apply() : IO[Deck] =
    IO.pure(
      (for {
        sp <- Shape.values
        c  <- Color.values
        n  <- Number.values
        sd <- Shading.values
      } yield Card(sp, c, n, sd)).toSet
  ).map((setCard : Set[Card]) => new Deck(Random.shuffle(setCard)))

  def drawCards(n : Int) : StateT[IO,Deck, Set[Card]] = StateT( (deck : Deck) =>
    if (deck.cards.size >= n)
      IO.pure((new Deck(deck.cards.drop(n)), deck.cards.take(n)))
    else Deck().flatMap(drawCards(n).run(_))
  )
}

object GameBoard {

  def apply(deck : Deck, players: Set[Player]) : IO[GameState] =
    Deck.drawCards(12).run(deck).map{ case (newDeck, cards) =>
      GameState(players, newDeck, new GameBoard(cards))
    }

  /**
    * Called to refill the board after a player successful set or after a round without sets
    */
  def refill(gamestate : GameState) : IO[GameState] =
    Deck.drawCards(3).run(gamestate.deck).map { case (newDeck, cards) =>
      GameState(gamestate.players, newDeck, new GameBoard(gamestate.board.cards ++ cards))
    }
}
