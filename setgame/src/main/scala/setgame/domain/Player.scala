package setgame.domain

import java.util.UUID
import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import monocle.Lens

case class Player(id : UUID, score : Int)

object Player {

  // Just select the first 3 card on the board an call check for a gameset
  val dumbPlayerStrategy : StateT[IO, GameState, Option[GameSet]] =
    StateT( (gamestate : GameState) => {
      val topThreeCards : Set[Card] =
        GameState.gameBoardCardsLens.get(gamestate).take(3)
      val setCandidate : Option[GameSet] = GameSet(topThreeCards)
      setCandidate match {
        case None => IO.pure((gamestate, setCandidate))
        case _ => {
          val tempGameboard : GameBoard =
            GameState.gameBoardLens.get(GameState.gameBoardCardsLens
              .modify(_.drop(3))(gamestate))
          GameBoard.refill(tempGameboard).map(
            (newGameboard : GameBoard) =>
            (gamestate.copy(board = newGameboard), setCandidate)
          ).runA(gamestate.deck)
        }
      }
    })

  // Take the game state
  // the player try to guess the set
  // player score update
  // gamestate update and returns
  def move(player : Player) : StateT[IO, GameState, Unit] = for {
    maybeGameset <- dumbPlayerStrategy
    newPlayer = if (maybeGameset.isDefined) player.copy(score = player.score + 1)
    else player
    resultGs <- StateT.get[IO, GameState]
    _ <- StateT.set[IO, GameState](
      if (resultGs.players(player)) {
        val players = (resultGs.players - player) + newPlayer
        resultGs.copy(players = players)
      } else resultGs.copy(players = resultGs.players + newPlayer)
    )
  } yield ()
}
