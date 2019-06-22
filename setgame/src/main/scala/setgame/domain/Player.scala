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
    ???
    // StateT { (gamestate : GameState) =>
    // val (c1, c2, c3) =
    //   (GameState.gameBoardCardsLens
    //     .get(gamestate)
    //     .take(3))
    // val set : Option[GameSet] =
    //   Applicative[Id].pure(GameSet.apply _)
    //     .map(_.tupled) <*> firstThreeBoardCards
    // if (set.isDefined)
    //   GameBoard.refill(GameState.gameBoardCardsLens.modify(_.drop(3))(gamestate))
    //     .map((_, set))
    // else
    //   IO.pure((gamestate, set))
    // }


  // Take the game state
  // the player try to guess the set
  // player score update
  // gamestate update and returns
  def move(player : Player, gamestate : GameState) : GameState = ???
}
