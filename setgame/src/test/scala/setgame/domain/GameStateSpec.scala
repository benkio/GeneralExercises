package setgame.domain

import org.scalatest._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import java.util.UUID

object GemeStateScalaCheckSpec extends Properties("GameState") {

  property("winner should select the player with the best score") =
    forAll(Gen.nonEmptyListOf(Gen.posNum[Int])) {
      (l : List[Int]) => {
        val inputPlayers = l.map(score => Player.apply.copy(score = score)).toSet
        val expectedWinner = inputPlayers.maxBy(_.score)
        val result = GameState.winner(GameState(
          inputPlayers,
          Deck(List.empty[Card]),
          GameBoard.build.runA(Deck(List.empty[Card])).unsafeRunSync
        ))
        result.isSuccess && result.get == expectedWinner
      }
    }

  property("playerScores should return the list of all scores") =
    forAll(Gen.listOf(Gen.posNum[Int])) {
      (l : List[Int]) => {
        val inputPlayers = l.map(score => Player.apply.copy(score = score)).toSet
        val result = GameState.playerScores(GameState(
          inputPlayers,
          Deck(List.empty[Card]),
          GameBoard.build.runA(Deck(List.empty[Card])).unsafeRunSync
        ))
        result == l || (result.forall(l.contains(_)) && result.diff(l).isEmpty)
      }
    }
}

class GameStateSpec extends WordSpec with Matchers {

  "Gamestate winner" should {
    "return a failure if the list of players is empty" in {
      GameState.winner(GameState(
        Set.empty[Player],
        Deck(List.empty[Card]),
          GameBoard.build.runA(Deck(List.empty[Card])).unsafeRunSync
        )).isFailure == true
    }
  }
}
