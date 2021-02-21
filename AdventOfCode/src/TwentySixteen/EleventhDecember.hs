{-# LANGUAGE OverloadedStrings #-}

module TwentySixteen.EleventhDecember where

import           Control.Concurrent
import           Data.List
import           Data.Map           (Map, adjust, elems, fromList,
                                     takeWhileAntitone, toList, traverseWithKey)
import qualified Data.Map           as Map (lookup)
import           Data.Maybe
import           Data.Ord
import           Data.Set           (Set, difference, empty, singleton, size)
import qualified Data.Set           as Set (elemAt, elems, filter, fromList,
                                            insert, map, null, toList, union)
import           System.IO

data RTG
  = Microchip String
  | Generator String
  deriving (Show, Eq, Ord)

type Floors = Map Int Floor

type Floor = Set RTG

data State =
  State
    { elevatorFloor :: Int
    , floors        :: Floors
    }
  deriving (Show)

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
floorsEquality :: (Floors, Int) -> (Floors, Int) -> Bool
floorsEquality (fs1, step1) (fs2, step2) =
  step1 >= step2 && all (null . uncurry difference) (elems fs1 `zip` elems fs2)

isMicrochip :: RTG -> Bool
isMicrochip (Microchip _) = True
isMicrochip _             = False

rtgElement :: RTG -> String
rtgElement (Microchip s) = s
rtgElement (Generator s) = s

changeRtgElement :: String -> RTG -> RTG
changeRtgElement s (Microchip _) = Microchip s
changeRtgElement s (Generator _) = Generator s

validateRTG :: RTG -> RTG -> Bool
validateRTG (Microchip _) (Microchip _)  = True
validateRTG (Generator _) (Generator _)  = True
validateRTG (Microchip s) (Generator s') = s == s'
validateRTG (Generator s) (Microchip s') = s == s'

elements :: Floors -> [String]
elements = Set.toList . foldl (\x y -> Set.union x (Set.map rtgElement y)) empty . elems

floorCombinations :: Floors -> [String] -> Int -> Set (Floors, Int)
floorCombinations flrs elements' step =
  ( Set.fromList
    . fmap (\x -> (x,step))
    . (flrs:)
    . filter (\x ->
      null (difference ((Set.fromList . elements) flrs)
            ((foldl (\a b -> Set.union a ((Set.map rtgElement . Set.filter isMicrochip) b)) empty . elems) x))
      && null (difference ((Set.fromList . elements) flrs)
            ((foldl (\a b -> Set.union a ((Set.map rtgElement . Set.filter (not . isMicrochip)) b)) empty . elems) x))
      )
   . traverseWithKey (\_ fl ->
                        equivalentFloors elements' fl)) flrs

equivalentFloors :: [String] -> Floor -> [Floor]
equivalentFloors elements' = fmap Set.fromList . traverse (\x -> fmap (`changeRtgElement` x) (delete (rtgElement x) elements')) . Set.toList

validateFloor :: Int -> Floor -> Floors -> Bool
validateFloor f l flrs = validateFloor' $ Set.union l $ (fromJust . Map.lookup f) flrs

validateFloor' :: Floor -> Bool
validateFloor' fls = (all (`validate` fls) . Set.filter isMicrochip) fls
  where
    validate :: RTG -> Floor -> Bool
    validate r@(Microchip _) rs =
      let generators = Set.filter (not . isMicrochip) rs
       in null generators || any (validateRTG r) generators
    validate r@(Generator _) rs =
      let chips = Set.filter isMicrochip rs
       in null chips || any (validateRTG r) chips

buildElevator :: RTG -> RTG -> Maybe Floor
buildElevator r r'
  | validateRTG r r' = (Just . Set.fromList) [r, r']
  | otherwise = Nothing

validLoads :: Floor -> ([Floor], [Floor])
validLoads fls =
  ( partition ((== 2)
    . size)
    . mapMaybe
     (\l ->
        if size l == 1
          then (Just . singleton) (Set.elemAt 0 l)
          else buildElevator (Set.elemAt 0 l) (Set.elemAt 1 l))
   . filter (\l -> ((\x -> x <= 2 && x > 0) . size) l && validateFloor' (difference fls  l))
   . fmap Set.fromList
   . subsequences
   . Set.toList
  )
    fls

endCondition :: State -> Bool
endCondition State {elevatorFloor = ef, floors = fs} =
  ef == 4 &&
  ((\l -> (and . fmap ((==0) . length . Set.elems . snd)  . filter ((<4) .  fst) ) l &&
    (not . Set.null . snd . fromJust . find ((4==) . fst)) l
  ) . toList)
  fs

buildNextSteps :: State -> Int -> Set (Floors, Int) -> Int -> Floor -> Maybe State
buildNextSteps State {elevatorFloor = f, floors = flrs} step his floorValue el =
  let newflv = adjust (`difference` el) f flrs
      newflv' = adjust (Set.union el) floorValue newflv
  in if validateFloor floorValue el flrs &&  floorValue /= f && (not . (\x -> any (floorsEquality (x, step)) his)) newflv'
     then Just State {elevatorFloor = floorValue, floors = newflv'}
     else Nothing

nextSteps :: State -> Int -> Set (Floors, Int) -> [State]
nextSteps s@State {elevatorFloor = f, floors = flrs} step his =
  let (previousFloor, currentFloor, nextFloor) =
        (max (f - 1) 1, fromJust (Map.lookup f flrs), min (f + 1) 4)
      (nextValidLoadsPair, nextValidLoadsSingle) = validLoads currentFloor
      nextLoad = mapMaybe (buildNextSteps s step his nextFloor) nextValidLoadsPair
      nextLoad' = if null nextLoad
        then mapMaybe (buildNextSteps s step his nextFloor) nextValidLoadsSingle
        else nextLoad
      nextLoad'' = if (or . fmap ((>0) . size) . elems . takeWhileAntitone (previousFloor >=)) flrs
        then mapMaybe (buildNextSteps s step his previousFloor) nextValidLoadsSingle
        else []
   in nextLoad' ++ nextLoad''

stepZero :: Floors -> State
stepZero fls = State {elevatorFloor = 1, floors = fls}

solution1 :: State -> Int -> MVar Int -> MVar (Set (Floors, Int)) -> [String] -> IO ()
solution1 st step resultMV historyMV elements' = do
  history <- takeMVar historyMV
  _ <- putMVar historyMV (Set.union (floorCombinations (floors st) elements' step) history)
  resultNotFound <- isEmptyMVar resultMV
  case (not resultNotFound, endCondition st) of
    (_, True) -> do
      previousResult <- tryTakeMVar resultMV
      putStrLn $ "found a Result: " ++ show step
      if isJust previousResult then putMVar resultMV (min step (fromJust previousResult)) else putMVar resultMV step
    (True, _) -> return ()-- do
      -- result <- takeMVar resultMV
      -- let isStepHigher = step >= result
      -- _ <- putMVar resultMV result
      -- if isStepHigher then return () else solution1Step st (step + 1) resultMV historyMV
    _    -> solution1Step st (step + 1) resultMV historyMV elements'

solution1Step :: State -> Int -> MVar Int -> MVar (Set (Floors, Int)) -> [String] -> IO ()
solution1Step st step resultMV historyMV elements' = do
  history <- takeMVar historyMV
  --putStrLn $ show st ++ " " ++ show step-- ++ " " ++ show history
--  when (step `mod` 50 == 0) $ putStr "*" >> hFlush stdout
  let nextStates = nextSteps st step history
  _ <- putMVar historyMV history
  mapM_ (\st' -> solution1 st' step resultMV historyMV elements') nextStates

solution1Test :: IO Bool
solution1Test = (== 11) <$> solution inputTest

solution :: Floors -> IO Int
solution i = do
  let firstState = stepZero i
      elemCombinations = elements inputTest
  resultMV <- newEmptyMVar
  historyMV <- newMVar empty
  _ <- solution1 firstState 0 resultMV historyMV elemCombinations
  takeMVar resultMV

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = input >>= solution

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = do
  i <- input
  let newInput = Set.fromList [Microchip "elerium", Microchip "dilithium", Generator "dilithium", Microchip "dilithium"]
      totalInput = adjust (Set.union newInput) 1 i
  solution totalInput
