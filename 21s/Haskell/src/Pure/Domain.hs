module Pure.Domain
  (
    Card (..),
    CardValue (..),
    CardType (..),
    TwentyOneValue (..),
    Player (..),
    GameState (..),
    deck,
    sam,
    dealer,
    Deck
  )
where

-------------------------------------------------------------------------------
--                                   Types                                   --
-------------------------------------------------------------------------------

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | King | Queen | Ace deriving (Show, Enum, Bounded, Eq, Ord)

data CardType = Clubs | Diamonds | Hearts | Spades deriving (Show, Bounded, Enum, Eq, Ord)

data Card = Card {
  cValue :: CardValue,
  cType :: CardType
  } deriving (Show, Eq, Ord)

data Player = Player {
  hand :: [Card],
  name :: String
  } deriving (Show, Eq)

type Deck = [Card]

data GameState = GameState {
  properPlayer :: Player,
  dealerPlayer :: Player,
  gameStateDeck   :: Deck
  } deriving (Show)
-------------------------------------------------------------------
--                           typeClasses                         --
-------------------------------------------------------------------

class (Enum a) => TwentyOneValue a where
  to21Value :: a -> Int


instance TwentyOneValue CardValue where
  to21Value Two    = 2
  to21Value Three  = 3
  to21Value Four   = 4
  to21Value Five   = 5
  to21Value Six    = 6
  to21Value Seven  = 7
  to21Value Eight  = 8
  to21Value Nine   = 9
  to21Value Ten    = 10
  to21Value Jack   = 10
  to21Value King   = 10
  to21Value Queen  = 10
  to21Value Ace    = 11

----------------------------------------------------------
--                    Starting Values                   --
----------------------------------------------------------

sam :: Player
sam = Player {hand=[], name="Sam"}

dealer :: Player
dealer = Player {hand=[], name="Dealer"}

deck :: Deck
deck = [Card {cValue=x, cType=y} | x <- [minBound..], y <- [minBound..]]