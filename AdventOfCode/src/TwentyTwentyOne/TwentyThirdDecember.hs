module TwentyTwentyOne.TwentyThirdDecember where

data Anphipod = Amber | Bronze | Copper | Desert deriving (Show)

data Room = A Space Space | B Space Space | C Space Space | D Space Space deriving (Show)

data Space = Empty | Occupied Anphipod | RoomEntry Room deriving (Show)

type Hallway = [Space]

energy :: Anphipod -> Int
energy Amber = 1
energy Bronze = 10
energy Copper = 100
energy Desert = 1000

-- Moves ------------------------------------------------------
exitRoomA :: Hallway -> [(Hallway, Int)]
exitRoomA = undefined

exitRoomB :: Hallway -> [(Hallway, Int)]
exitRoomB = undefined

exitRoomC :: Hallway -> [(Hallway, Int)]
exitRoomC = undefined

exitRoomD :: Hallway -> [(Hallway, Int)]
exitRoomD = undefined

enterRoomA :: Hallway -> [(Hallway, Int)]
enterRoomA = undefined

enterRoomB :: Hallway -> [(Hallway, Int)]
enterRoomB = undefined

enterRoomC :: Hallway -> [(Hallway, Int)]
enterRoomC = undefined

enterRoomD :: Hallway -> [(Hallway, Int)]
enterRoomD = undefined

input :: IO String
input = readFile "input/2021/23December.txt"

twentyThirdDecemberSolution1 :: IO Int
twentyThirdDecemberSolution1 = undefined

twentyThirdDecemberSolution2 :: IO Int
twentyThirdDecemberSolution2 = undefined

-- Utilities -----------------------------------
getRoomA :: Hallway -> Room
getRoomA h = unsafeGetRoom $ h !! 2

getRoomB :: Hallway -> Room
getRoomB h = unsafeGetRoom $ h !! 4

getRoomC :: Hallway -> Room
getRoomC h = unsafeGetRoom $ h !! 6

getRoomD :: Hallway -> Room
getRoomD h = unsafeGetRoom $ h !! 8

unsafeGetRoom :: Space -> Room
unsafeGetRoom (RoomEntry r) = r

inputHallway :: Hallway
inputHallway =
  [ Empty,
    Empty,
    RoomEntry (A (Occupied Amber) (Occupied Copper)),
    Empty,
    RoomEntry (B (Occupied Desert) (Occupied Desert)),
    Empty,
    RoomEntry (C (Occupied Copper) (Occupied Bronze)),
    Empty,
    RoomEntry (D (Occupied Amber) (Occupied Bronze)),
    Empty,
    Empty
  ]

testHallway :: Hallway
testHallway =
  [ Empty,
    Empty,
    RoomEntry (A (Occupied Bronze) (Occupied Amber)),
    Empty,
    RoomEntry (B (Occupied Copper) (Occupied Desert)),
    Empty,
    RoomEntry (C (Occupied Bronze) (Occupied Copper)),
    Empty,
    RoomEntry (D (Occupied Desert) (Occupied Amber)),
    Empty,
    Empty
  ]
