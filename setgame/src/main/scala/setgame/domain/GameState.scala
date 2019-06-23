package setgame.domain

import monocle.Lens
import monocle.macros.GenLens
import scala.util.Try

case class GameState(
  players : Set[Player],
  deck: Deck,
  board : GameBoard
)

object GameState {
  val deckLens : Lens[GameState, Deck] = GenLens[GameState](_.deck)
  val gameBoardLens : Lens[GameState, GameBoard] = GenLens[GameState](_.board)
  val gameBoardCardsLens : Lens[GameState, Set[Card]] =
    (gameBoardLens composeLens GameBoard.lens)
  val deckCardsLens : Lens[GameState, Set[Card]] =
    (deckLens composeLens Deck.lens)

  def winner(gs : GameState) : Try[Player] =
    Try(gs.players.maxBy(_.score))
}
