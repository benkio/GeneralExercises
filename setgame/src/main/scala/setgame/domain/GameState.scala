package setgame.domain

case class GameState(
  players : Set[Player],
  deck: Deck,
  board : GameBoard
)
