{-# LANGUAGE OverloadedStrings #-}

module TwentySixteen.EleventhDecemberBis where

import Control.Concurrent
import Data.List
import Data.Map (Map, elems, foldrWithKey, fromList, toList)
import qualified Data.Map as Map (foldl, insert, lookup, takeWhileAntitone, traverseWithKey)
import Data.Maybe
import Data.Ord
import Data.Set (Set, difference, empty, singleton, size)
import qualified Data.Set as Set
  ( elemAt
  , elems
  , filter
  , fromList
  , insert
  , map
  , null
  , toList
  , union
  )
import System.IO

data RTG
  = Microchip String
  | Generator String
  deriving (Show, Eq, Ord)

type Floors = Map Int Floor

type Floor = Set RTG

data State =
  State
    { elevatorFloor :: Int
    , floors :: Floors
    }
  deriving (Show, Eq)

-- Input ----------------------------------------------------------------------
input :: IO Floors
input = parseInput <$> readFile "input/2016/11December.txt"

parseInput :: String -> Floors
parseInput =
  fromList .
  ([1 ..] `zip`) .
  fmap
    (parseRTG .
     filter (`notElem` ["and", "a", "nothing", "relevant."]) . drop 4 . words) .
  lines

parseRTG :: [String] -> Floor
parseRTG [] = empty
parseRTG (x:y:xs) =
  if "microchip" `isPrefixOf` y
    then Set.insert (Microchip (takeWhile ('-' /=) x)) $ parseRTG xs
    else Set.insert (Generator x) $ parseRTG xs

inputTest :: Floors
inputTest =
  parseInput
    "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n\
\The second floor contains a hydrogen generator.\n\
\The third floor contains a lithium generator.\n\
\The fourth floor contains nothing relevant."

-- ----- ------------------------------------------------------
isMicrochip :: RTG -> Bool
isMicrochip (Microchip _) = True
isMicrochip _ = False

rtgElement :: RTG -> String
rtgElement (Microchip s) = s
rtgElement (Generator s) = s

changeRtgElement :: String -> RTG -> RTG
changeRtgElement s (Microchip _) = Microchip s
changeRtgElement s (Generator _) = Generator s

validateRTG :: RTG -> RTG -> Bool
validateRTG (Microchip _) (Microchip _) = True
validateRTG (Generator _) (Generator _) = True
validateRTG (Microchip s) (Generator s') = s == s'
validateRTG (Generator s) (Microchip s') = s == s'

elementsToFloor :: Int -> Floors -> [RTG]
elementsToFloor floorNum =
  foldl (\x y -> x ++ Set.toList y) [] .
  elems . Map.takeWhileAntitone (\k -> k < floorNum)

buildElevator :: RTG -> RTG -> Maybe Floor
buildElevator r r'
  | validateRTG r r' = (Just . Set.fromList) [r, r']
  | otherwise = Nothing

buildInitialState :: Floors -> State
buildInitialState fls = State {elevatorFloor = 1, floors = fls}

floorCombinations :: State -> [State]
floorCombinations State {elevatorFloor = l, floors = fls } =
  (fmap (\y -> State { elevatorFloor = l, floors = y })
    . (fls:)
    . filter (\x ->
      null (difference (Set.fromList elements)
            ((foldl (\a b -> Set.union a ((Set.map rtgElement . Set.filter isMicrochip) b)) empty . elems) x))
      && null (difference (Set.fromList elements)
            ((foldl (\a b -> Set.union a ((Set.map rtgElement . Set.filter (not . isMicrochip)) b)) empty . elems) x))
      )
   . Map.traverseWithKey (\_ fl ->
                        equivalentFloors elements fl)) fls
  where elements = (nub . fmap rtgElement . elementsToFloor 5) fls

equivalentFloors :: [String] -> Floor -> [Floor]
equivalentFloors elements' = fmap Set.fromList . traverse (\x -> fmap (`changeRtgElement` x) (delete (rtgElement x) elements')) . Set.toList

validLoads :: Floor -> [Floor]
validLoads fls =
  (mapMaybe
     (\l ->
        if size l == 1
          then (Just . singleton) (Set.elemAt 0 l)
          else buildElevator (Set.elemAt 0 l) (Set.elemAt 1 l)) .
   filter
     (\l ->
        ((\x -> x <= 2 && x > 0) . size) l && validateFloor (difference fls l)) .
   fmap Set.fromList . subsequences . Set.toList)
    fls

validateFloor :: Floor -> Bool
validateFloor fls = (all (`validate` fls) . Set.filter isMicrochip) fls
  where
    validate :: RTG -> Floor -> Bool
    validate r@(Microchip _) rs =
      let generators = Set.filter (not . isMicrochip) rs
       in null generators || any (validateRTG r) generators
    validate r@(Generator _) rs =
      let chips = Set.filter isMicrochip rs
       in null chips || any (validateRTG r) chips

validateFloors :: Floors -> Bool
validateFloors = Map.foldl (\acc f -> acc && validateFloor f) True

nextStates :: State -> [State]
nextStates st@State {elevatorFloor = el, floors = fls} =
  let nextFloorNums =
        if length (elementsToFloor el fls) == 0
          then [min 4 (el + 1)]
          else [max 1 (el - 1), min 4 (el + 1)]
   in (concatMap (\x -> nextStates' x st) . filter (el /=)) nextFloorNums

nextStates' :: Int -> State -> [State]
nextStates' floorNum State {elevatorFloor = el, floors = fls} =
  let ls =
        (filter
           (not .
            (\x -> x == 2 && floorNum < el) .
            length) .
         validLoads) $
        (fromJust . Map.lookup el) fls
   in (sortStates .
       fmap (\y -> State {elevatorFloor = floorNum, floors = y}) .
       filter validateFloors . fmap (moveload fls el floorNum))
        ls

moveload :: Floors -> Int -> Int -> Floor -> Floors
moveload fls currentFloorNum nextFloorNum load =
  let currentFloor =
        difference ((fromJust . Map.lookup currentFloorNum) fls) load
      nextFloor = Set.union ((fromJust . Map.lookup nextFloorNum) fls) load
      fls' = Map.insert currentFloorNum currentFloor fls
   in Map.insert nextFloorNum nextFloor fls'

sortStates :: [State] -> [State]
sortStates = sortOn (Data.Ord.Down . stateScore)

stateScore :: State -> Int
stateScore State {elevatorFloor = el, floors = fls} =
  let floorsScore = foldrWithKey (\k f acc -> acc + (k * size f)) 0 fls
   in el + floorsScore

endCondition :: State -> Bool
endCondition State {elevatorFloor = ef, floors = fs} =
  ef == 4 &&
  ((\l ->
      (and . fmap ((== 0) . length . Set.elems . snd) . filter ((< 4) . fst)) l &&
      (not . Set.null . snd . fromJust . find ((4 ==) . fst)) l) .
   toList)
    fs

-- searchSolution' :: State -> [(State, [State])] -> Int
-- searchSolution' st chain
--   -- | endCondition st = length chain -- ++ [(st, ns)]
--   -- | null ns || st `elem` (fmap fst chain) = backtrack st chain
--   -- | otherwise = searchSolution (head ns) (chain ++ [(st, ns)])
--   -- where
--   --   ns = nextStates st

-- backtrack :: State -> [(State, [State])] -> Int
-- backtrack st chain =
--   let (previousState, previousNextStates) = last chain
--       previousNextStates' = delete st previousNextStates
--    in if null previousNextStates'
--         then backtrack previousState (init chain)
--         else searchSolution
--                (head previousNextStates')
--                ((init chain) ++ [(previousState, previousNextStates')])

searchSolution :: [State] -> [[State]] -> Int
searchSolution st chain
  | any endCondition st = length chain -- ++ [(st, ns)]
  | otherwise =  searchSolution ((filter (`notElem` (concat chain)) . concatMap nextStates) st) (chain ++ [concatMap floorCombinations st])

solution1Test :: Bool
solution1Test = searchSolution [buildInitialState inputTest] [] == 11

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = (`searchSolution` []) . (:[]) . buildInitialState <$> input

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = undefined
