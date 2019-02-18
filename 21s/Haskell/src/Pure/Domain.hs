module Pure.Domain
  (
    Card (..),
    CardValue (..),
    CardType (..),
    TwentyOneValue (..)
  )
where

import Data.Set

-------------------------------------------------------------------------------
--                                   Types                                   --
-------------------------------------------------------------------------------

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | King | Queen | Ace deriving (Show, Enum, Bounded)

data CardType = Club | Diamond | Heart | Spade deriving (Show)

data Card = Card {
  cValue :: CardValue,
  cType :: CardType
  } deriving (Show)

data Player = Player {
  hand :: Set[Card],
  name :: String
  } deriving (Show)

type Deck = Set[Card]

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
sam = Player {hand=empty, name="Sam"}

dealer :: Player
dealer = Player {hand=empty, name="Dealer"}

deck :: Deck
deck = undefined