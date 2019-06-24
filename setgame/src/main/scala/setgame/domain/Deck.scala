package setgame.domain

import scala.util.Random
import cats.effect.IO
import cats.data._
import monocle.Lens
import monocle.macros.GenLens

//No private constructor for testing purposes
case class Deck(cards: List[Card])
case class GameBoard private(cards: List[Card])

object Deck {

  val lens : Lens[Deck, List[Card]] = GenLens[Deck](_.cards)

  def apply() : IO[Deck] =
    IO.pure(
      (for {
        sp <- Shape.values
        c  <- Color.values
        n  <- Number.values
        sd <- Shading.values
      } yield Card(sp, c, n, sd))
  ).map((setCard : List[Card]) => new Deck(Random.shuffle(setCard)))

  def drawCards(n : Int) : StateT[IO,Deck, List[Card]] = StateT( (deck : Deck) =>
    if (deck.cards.size >= n)
      IO.pure((new Deck(deck.cards.drop(n)), deck.cards.take(n)))
    else Deck().flatMap(drawCards(n).run(_))
  )
}

object GameBoard {

  val lens : Lens[GameBoard, List[Card]] = GenLens[GameBoard](_.cards)

  val build : StateT[IO, Deck, GameBoard] =
    Deck.drawCards(12).map((cards) => new GameBoard(cards))

  //Warning, loop if n > 12
  def drawCards(deck : Deck, n : Int = 1) : StateT[IO, GameBoard, List[Card]] =
    StateT( (board : GameBoard) =>
    if (board.cards.size >= n)
      IO.pure((new GameBoard(board.cards.drop(n)), board.cards.take(n)))
    else GameBoard.build.run(deck)
      .flatMap {
        case (deck, newBoard) => drawCards(deck, n).run(newBoard)
      }
  )

  /**
    * Called to refill the board after a player successful set or after a round without sets
    */
  def refill(board : GameBoard) : StateT[IO, Deck, GameBoard] =
    Deck.drawCards(3).map((cards : List[Card]) =>
      new GameBoard(board.cards ++ cards)
    )
}
