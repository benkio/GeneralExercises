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

  // Take the game state
  // the player try to guess the set
  // player score update
  // gamestate update and returns
  def move(player : Player, gamestate : GameState) : GameState = ???
}
