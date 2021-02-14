{-# LANGUAGE OverloadedStrings #-}
module TwentySixteen.EleventhDecember where

import           Data.List
import           Data.Map   (Map, fromList, toList)
import qualified Data.Map   as Map (insert, lookup)
import           Data.Maybe

data RTG
  = Microchip String
  | Generator String
  deriving (Show, Eq)

type Floors = Map Int [RTG]

type Floor = [RTG]

data Elevator =
  Elevator
    { efloor :: Int
    , load   :: Elevatorload
    } deriving (Eq,Show)

data Elevatorload
  = Elevatorload RTG
  | ElevatorloadUnsafe RTG RTG -- Read-only
  deriving (Show, Eq)

elevatorloadToList :: Elevatorload -> [RTG]
elevatorloadToList (Elevatorload r)          = [r]
elevatorloadToList (ElevatorloadUnsafe r r') = [r, r']

removeLoadFromFloor :: Floor -> Elevatorload -> Floor
removeLoadFromFloor f l = f \\ elevatorloadToList l

isMicrochip :: RTG -> Bool
isMicrochip (Microchip _) = True
isMicrochip _             = False

validateRTG :: RTG -> RTG -> Bool
validateRTG (Microchip _) (Microchip _)  = True
validateRTG (Generator _) (Generator _)  = True
validateRTG (Microchip s) (Generator s') = s == s'
validateRTG (Generator s) (Microchip s') = s == s'

validateFloor :: Elevator -> Floors -> Bool
validateFloor Elevator {efloor = f, load = l} floors =
  validateFloor' $ elevatorloadToList l ++ (fromJust . Map.lookup f) floors

validateFloor' :: [RTG] -> Bool
validateFloor' [] = True
validateFloor' (r@(Microchip _):rs) =
  let generators = filter (not . isMicrochip) rs
   in (null generators || any (validateRTG r) generators) && validateFloor' rs
validateFloor' (r@(Generator _):rs) =
  let chips = filter isMicrochip rs
   in (null chips || any (validateRTG r) chips) && validateFloor' rs

buildElevator :: RTG -> RTG -> Maybe Elevatorload
buildElevator r r'
  | validateRTG r r' = Just $ ElevatorloadUnsafe r r'
  | otherwise = Nothing

validLoads :: Floor -> [Elevatorload]
validLoads =
  mapMaybe
    (\l ->
       if length l == 1
         then Just (Elevatorload (head l))
         else buildElevator (head l) (last l)) .
  filter ((`elem` [1, 2]) . length) . subsequences

endCondition :: Elevator -> Floors -> Bool
endCondition e fs =
  efloor e == 4 &&
  ((\l -> (null . concat . init) l && (not . null . last) l) . fmap snd . toList) fs

nextSteps :: Elevator -> Floors -> [(Elevator, Floors)]
nextSteps Elevator {efloor = f, load = l} floors = undefined

stepZero :: Floors -> (Elevator, Floors)
stepZero floors =
  let firstFloor = (fromJust . Map.lookup 1) floors
      firstElevator = Elevator { efloor = 1, load = Elevatorload (head firstFloor)}
  in (firstElevator, Map.insert 1 (tail firstFloor) floors)

solution1 :: [(Elevator,Floors)] -> [(Elevator,Floors)] -> Int -> Int
solution1 state history step = undefined

input :: IO Floors
input = parseInput <$> readFile "input/2016/11December.txt"

parseInput :: String -> Floors
parseInput =
   fromList
  . ([1..] `zip`)
  . fmap (parseRTG . filter (`notElem` ["and", "a", "nothing", "relevant."]) . drop 4 . words)
  . lines

parseRTG :: [String] -> [RTG]
parseRTG [] = []
parseRTG (x:y:xs) = if "microchip" `isPrefixOf` y
             then Microchip ((fst . break ('-' ==)) x) : parseRTG xs
             else Generator x : parseRTG xs


inputTest :: Floors
inputTest = parseInput "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n\
\The second floor contains a hydrogen generator.\n\
\The third floor contains a lithium generator.\n\
\The fourth floor contains nothing relevant."

solution1Test :: Bool
solution1Test = ((\l -> solution1 l [] 0) . (:[]) . stepZero) inputTest == 11

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = (\l -> solution1 l [] 0) . (:[]) . stepZero <$> input

solution2 :: String -> Int
solution2 = undefined

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = undefined
