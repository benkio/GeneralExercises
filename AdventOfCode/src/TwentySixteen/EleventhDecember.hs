{-# LANGUAGE OverloadedStrings #-}

module TwentySixteen.EleventhDecember where

import           Data.List
import           Data.Map   (Map, elems, fromList, toList)
import qualified Data.Map   as Map (insert, lookup)
import           Data.Maybe
import           Data.Ord

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
    }
  deriving (Eq, Show)

data State = State { elevator :: Elevator, floors:: Floors, step:: Int, history :: [Floors]} deriving Show
data Elevatorload
  = Elevatorload RTG
  | ElevatorloadUnsafe RTG RTG -- Read-only
  deriving (Show, Eq)

floorsEquality :: Floors -> Floors -> Bool
floorsEquality fs1 fs2 =
  all (null . uncurry (\\)) $ elems fs1 `zip` elems fs2

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

unloadElevator :: Elevator -> Floors -> Floors
unloadElevator e fls = Map.insert (efloor e) (unloadElevatorload (efloor e) (load e) fls) fls

unloadElevatorload :: Int -> Elevatorload -> Floors -> Floor
unloadElevatorload f l floors = elevatorloadToList l ++ (fromJust . Map.lookup f) floors

validateFloor :: Int -> Elevatorload -> Floors -> Bool
validateFloor f l floors =
  validateFloor' $ unloadElevatorload f l floors

validateFloor' :: [RTG] -> Bool
validateFloor' fls = (all (\rtg -> validate rtg fls) . filter isMicrochip) fls
  where
    validate :: RTG -> [RTG] -> Bool
    validate r@(Microchip _) rs =
      let generators = filter (not . isMicrochip) rs
       in null generators || any (validateRTG r) generators
    validate r@(Generator _) rs =
      let chips = filter isMicrochip rs
       in null chips || any (validateRTG r) chips

buildElevator :: RTG -> RTG -> Maybe Elevatorload
buildElevator r r'
  | validateRTG r r' = Just $ ElevatorloadUnsafe r r'
  | otherwise = Nothing

validLoads :: Floor -> [Elevatorload]
validLoads fls =
  ( mapMaybe
    (\l ->
       if length l == 1
         then Just (Elevatorload (head l))
         else buildElevator (head l) (last l))
  . filter (\l -> ((`elem` [1, 2]) . length) l && validateFloor' (fls \\ l))
  . subsequences) fls

endCondition :: Elevator -> Floors -> Bool
endCondition e fs =
  efloor e == 4 &&
  ((\l -> (null . concat . init) l && (not . null . last) l) . fmap snd . toList)
    fs

nextSteps :: Elevator -> Floors -> [Floors] -> [(Elevator, Floors)]
nextSteps Elevator {efloor = f, load = l} floors his =
  let (previousFloor, currentFloor, nextFloor) =
        (max (f - 1) 1, fromJust (Map.lookup f floors), min (f + 1) 4)
      floorContent = currentFloor ++ elevatorloadToList l
      nextValidLoads =
        (sortOn (Data.Ord.Down . length . elevatorloadToList) . validLoads)
          floorContent
      buildNextSteps =
        \floorValue el ->
          ( Elevator {efloor = floorValue, load = el}
          , Map.insert f (removeLoadFromFloor floorContent el) floors)

      nextLoadUp =
        ( filter (not . (\x -> any (`floorsEquality` x) his) . uncurry unloadElevator)
          . fmap (buildNextSteps nextFloor)
          . filter (\el -> nextFloor /= f && validateFloor nextFloor el floors)) nextValidLoads
      nextLoadDown =
        ( filter (not . (\x -> any (`floorsEquality` x) his) . uncurry unloadElevator)
          . fmap (buildNextSteps previousFloor)
          . filter (\el -> previousFloor /= f && validateFloor previousFloor el floors)
          . reverse
        )
        nextValidLoads
   in if null nextLoadUp then nextLoadDown else nextLoadUp

higherFloorIndex :: Elevator -> Floors -> Int
higherFloorIndex Elevator { efloor = f, load = el} fls =
  ((f *) . length . elevatorloadToList) el +
  (sum . fmap (\(i,l) -> length l * i) . toList) fls

stepZero :: Floors -> State
stepZero fls =
  let firstFloor = (fromJust . Map.lookup 1) fls
      firstElevator =
        Elevator {efloor = 1, load = Elevatorload (head firstFloor)}
   in State { elevator = firstElevator, floors = Map.insert 1 (tail firstFloor) fls, step = 0, history = []}

solutionStep :: State -> [State]
solutionStep State { elevator = e, floors = fls, step = s, history = his} =
  let
    his' = [unloadElevator e fls]
    nextStep = -- maximumBy (\(e', x) (e'', x')-> compare (higherFloorIndex e' x) (higherFloorIndex e'' x')) $
      nextSteps e fls his

  in fmap (\(ens, fns) -> State { elevator = ens, floors = fns, step = s + 1, history = his ++ his' }) nextStep


solution1 :: [State] -> Int
solution1 sts --st@State { elevator = e, floors = fls, step = s }
  | isJust end = (step . fromJust) end
  | otherwise = solution1 $ concatMap solutionStep sts
  where end = find (\st -> endCondition (elevator st) (floors st)) sts

-- solution1Index :: Int -> State  -> State
-- solution1Index index st@State { elevator = e, floors = fls, step = s }
--   | index == 0 = st
--   | otherwise = solution1Index (index - 1) (solutionStep st)

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

parseRTG :: [String] -> [RTG]
parseRTG [] = []
parseRTG (x:y:xs) =
  if "microchip" `isPrefixOf` y
    then Microchip ((fst . break ('-' ==)) x) : parseRTG xs
    else Generator x : parseRTG xs

inputTest :: Floors
inputTest =
  parseInput
    "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.\n\
\The second floor contains a hydrogen generator.\n\
\The third floor contains a lithium generator.\n\
\The fourth floor contains nothing relevant."

solution1Test :: Bool
solution1Test = (solution1 . (:[])  . stepZero) inputTest == 11

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 =
  solution1 . (:[]) . stepZero <$> input

solution2 :: String -> Int
solution2 = undefined

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = undefined
