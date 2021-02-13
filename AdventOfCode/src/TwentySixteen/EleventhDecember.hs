module TwentySixteen.EleventhDecember where

import Data.List
import Data.Maybe

data RTG = Microchip String | Generator String deriving (Show)
type Floor = (Int, [RTG])
data Elevator = Elevator {
  floor :: Int,
  load :: Elevatorload
}
data Elevatorload =
  Elevatorload RTG
  | ElevatorloadUnsafe RTG RTG -- not to be used

isMicrochip :: RTG -> Bool
isMicrochip (Microchip _) = True
isMicrochip _ = False

validateRTG :: RTG -> RTG -> Bool
validateRTG (Microchip _) (Microchip _) = True
validateRTG (Generator _) (Generator _) = True
validateRTG (Microchip s) (Generator s') = s == s'
validateRTG (Generator s) (Microchip s') = s == s'

validateFloor :: [RTG] -> Bool
validateFloor [] = False
validateFloor (r@(Microchip _):rs) =
  let generators = filter (not . isMicrochip) rs
  in (null generators || any (validateRTG r) generators) && validateFloor rs
validateFloor (r@(Generator _):rs) =
  let chips = filter isMicrochip rs
  in (null chips || any (validateRTG r) chips) && validateFloor rs

buildElevator :: RTG -> RTG -> Maybe Elevatorload
buildElevator r r'
  | validateRTG r r' = Just $ ElevatorloadUnsafe r r'
  | otherwise = Nothing

validLoads :: Floor -> [Elevatorload]
validLoads (_, stuff) = (mapMaybe (\l -> if length l == 1
                                    then Just (Elevatorload (head l))
                                    else buildElevator (head l) (last l))
                         . filter ((`elem` [1,2] ) . length)
                         . subsequences) stuff

input :: IO String
input = readFile "input/2016/11December.txt"

solution1 :: String -> Int
solution1 = undefined

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = undefined

solution2 :: String -> Int
solution2 = undefined

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = undefined
