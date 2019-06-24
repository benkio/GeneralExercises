package setgame.domain

import monocle._
import monocle.macros.GenLens
import scala.util.Try
import scalaz.std.list._

case class GameState(
  players : Set[Player],
  deck: Deck,
  board : GameBoard
)

object GameState {
  val deckLens : Lens[GameState, Deck] = GenLens[GameState](_.deck)
  val gameBoardLens : Lens[GameState, GameBoard] = GenLens[GameState](_.board)
  val playersLens : Lens[GameState, Set[Player]] = GenLens[GameState](_.players)
  val gameBoardCardsLens : Lens[GameState, List[Card]] =
    (gameBoardLens composeLens GameBoard.lens)
  val deckCardsLens : Lens[GameState, List[Card]] =
    (deckLens composeLens Deck.lens)

  def winner(gs : GameState) : Try[Player] =
    Try(gs.players.maxBy(_.score))

  val playerScoresLens : Getter[GameState, List[Int]] =
    (playersLens composeGetter Getter[Set[Player], List[Int]](_.toList.map(_.score)))

  val playerScores : (GameState) => List[Int] = playerScoresLens.get _
}
