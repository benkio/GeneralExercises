package setgame.domain

import java.util.UUID

case class Player(id : UUID, score : Int)

object Player {

  // Just select the first 3 card on the board an call check for a gameset
  def dumbPlayerStrategy(gamestate : GameState) : (Option[GameSet], GameState) = ???

  // Take the game state
  // the player try to guess the set
  // player score update
  // gamestate update and returns
  def move(player : Player, gamestate : GameState) : GameState = ???
}
