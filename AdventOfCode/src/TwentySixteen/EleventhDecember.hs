{-# LANGUAGE OverloadedStrings #-}
module TwentySixteen.EleventhDecember where

import Data.List
import Data.Text (splitOn, pack, unpack)
import Data.Map (Map, findMax, fromList, toList)
import qualified Data.Map as Map (insert, lookup)
import Data.Maybe

data RTG
  = Microchip String
  | Generator String
  deriving (Show, Eq)

type Floors = Map Int [RTG]

type Floor = [RTG]

data Elevator =
  Elevator
    { efloor :: Int
    , load :: Elevatorload
    } deriving Show

data Elevatorload
  = Elevatorload RTG
  | ElevatorloadUnsafe RTG RTG -- Read-only
  deriving Show

elevatorloadToList :: Elevatorload -> [RTG]
elevatorloadToList (Elevatorload r) = [r]
elevatorloadToList (ElevatorloadUnsafe r r') = [r, r']

removeLoadFromFloor :: Floor -> Elevatorload -> Floor
removeLoadFromFloor f l = f \\ elevatorloadToList l

isMicrochip :: RTG -> Bool
isMicrochip (Microchip _) = True
isMicrochip _ = False

validateRTG :: RTG -> RTG -> Bool
validateRTG (Microchip _) (Microchip _) = True
validateRTG (Generator _) (Generator _) = True
validateRTG (Microchip s) (Generator s') = s == s'
validateRTG (Generator s) (Microchip s') = s == s'

validateFloor :: Elevator -> Floors -> Bool
validateFloor Elevator {efloor = f, load = l} floors =
  validateFloor' $ elevatorloadToList l ++ (fromJust . Map.lookup f) floors

validateFloor' :: [RTG] -> Bool
validateFloor' [] = False
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
  efloor e == (fst . findMax) fs &&
  ((\l -> (null . concat . init) l && (not . null . last) l) . fmap snd . toList) fs

nextSteps :: Elevator -> Floors -> [(Elevator, Floors)]
nextSteps Elevator {efloor = f, load = l} floors =
  let floorContent = fromJust (Map.lookup f floors)
      nextValidLoads =
        fmap (\el -> (el, removeLoadFromFloor floorContent el)) . validLoads $
        floorContent ++ elevatorloadToList l
   in (filter (uncurry validateFloor) .
       concatMap
         (\(el, nf) ->
            [ ( Elevator
                  {efloor = min (f + 1) ((fst . findMax) floors), load = el}
              , Map.insert f nf floors)
            , ( Elevator {efloor = max (f - 1) 1, load = el}
              , Map.insert f nf floors)
            ]))
        nextValidLoads

stepZero :: Floors -> (Elevator, Floors)
stepZero floors =
  let firstFloor = (fromJust . Map.lookup 1) floors
      firstElevator = Elevator { efloor = 1, load = Elevatorload (head firstFloor)}
  in (firstElevator, Map.insert 1 (tail firstFloor) floors)

solution1 :: [(Elevator,Floors)] -> Int -> Int
solution1 state step =
  let nextSteps' = concatMap (uncurry nextSteps) state
  in if any (uncurry endCondition) nextSteps'
      then step + 1
      else solution1 nextSteps' (step + 1)

input :: IO Floors
input = parseInput <$> readFile "input/2016/11December.txt"

parseInput :: String -> Floors
parseInput =
   fromList
  . ([1..] `zip`)
  . fmap (concatMap (buildRTG . unpack) . splitOn ", " . pack . unwords . filter (`notElem` ["and", "a", "nothing", "relevant."]) . drop 4 . words)
  . lines
  where buildRTG :: String -> [RTG]
        buildRTG [] = []
        buildRTG s = ((\l -> if "microchip" `isPrefixOf` last l
                          then [Microchip ((fst . break ('-' ==) . head) l)]
                          else [Generator (head l)]
                   ) . words) s

inputTest :: Floors
inputTest = parseInput "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n\
\The second floor contains a hydrogen generator.\n\
\The third floor contains a lithium generator.\n\
\The fourth floor contains nothing relevant."

solution1Test :: Bool
solution1Test = ((`solution1` 0) . (:[]) . stepZero) inputTest == 11

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = (`solution1` 0) . (:[]) . stepZero <$> input

solution2 :: String -> Int
solution2 = undefined

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = undefined
