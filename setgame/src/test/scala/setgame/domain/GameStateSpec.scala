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
        val inputPlayers = l.map(score => Player(UUID.randomUUID, score)).toSet
        val expectedWinner = inputPlayers.maxBy(_.score)
        val result = GameState.winner(GameState(
          inputPlayers,
          Deck(Set.empty[Card]),
          GameBoard.build.runA(Deck(Set.empty[Card])).unsafeRunSync
        ))
        result.isSuccess && result.get == expectedWinner
      }
    }
}

class GameStateSpec extends WordSpec with Matchers {

  "Gamestate winner" should {
    "return a failure if the list of players is empty" in {
      GameState.winner(GameState(
        Set.empty[Player],
        Deck(Set.empty[Card]),
          GameBoard.build.runA(Deck(Set.empty[Card])).unsafeRunSync
        )).isFailure == true
    }
  }
}
