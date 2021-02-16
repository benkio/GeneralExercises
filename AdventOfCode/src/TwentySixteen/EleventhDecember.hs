{-# LANGUAGE OverloadedStrings #-}

module TwentySixteen.EleventhDecember where

import           Control.Concurrent
import           Control.Monad
import           Data.List
import           Data.Map           (Map, adjust, elems, fromList,
                                     takeWhileAntitone, toList)
import qualified Data.Map           as Map (lookup)
import           Data.Maybe
import           Data.Ord
import           Data.Set           (Set, empty)
import qualified Data.Set           as Set (insert)
import           System.IO

data RTG
  = Microchip String
  | Generator String
  deriving (Show, Eq, Ord)

type Floors = Map Int [RTG]

type Floor = [RTG]

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

parseRTG :: [String] -> [RTG]
parseRTG [] = []
parseRTG (x:y:xs) =
  if "microchip" `isPrefixOf` y
    then Microchip (takeWhile ('-' /=) x) : parseRTG xs
    else Generator x : parseRTG xs

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
  step1 >= step2 && all (null . uncurry (\\)) (elems fs1 `zip` elems fs2)

isMicrochip :: RTG -> Bool
isMicrochip (Microchip _) = True
isMicrochip _             = False

validateRTG :: RTG -> RTG -> Bool
validateRTG (Microchip _) (Microchip _)  = True
validateRTG (Generator _) (Generator _)  = True
validateRTG (Microchip s) (Generator s') = s == s'
validateRTG (Generator s) (Microchip s') = s == s'

validateFloor :: Int -> Floor -> Floors -> Bool
validateFloor f l flrs = validateFloor' $ l ++ (fromJust . Map.lookup f) flrs

validateFloor' :: [RTG] -> Bool
validateFloor' fls = (all (`validate` fls) . filter isMicrochip) fls
  where
    validate :: RTG -> [RTG] -> Bool
    validate r@(Microchip _) rs =
      let generators = filter (not . isMicrochip) rs
       in null generators || any (validateRTG r) generators
    validate r@(Generator _) rs =
      let chips = filter isMicrochip rs
       in null chips || any (validateRTG r) chips

buildElevator :: RTG -> RTG -> Maybe Floor
buildElevator r r'
  | validateRTG r r' = Just [r, r']
  | otherwise = Nothing

validLoads :: Floor -> ([Floor], [Floor])
validLoads fls =
  ( partition ((== 2)
    . length)
    . mapMaybe
     (\l ->
        if length l == 1
          then Just [head l]
          else buildElevator (head l) (last l)) .
   filter (\l -> ((`elem` [1, 2]) . length) l && validateFloor' (fls \\ l)) .
   subsequences)
    fls

endCondition :: State -> Bool
endCondition State {elevatorFloor = ef, floors = fs} =
  ef == 4 &&
  ((\l -> (null . concat . init) l && (not . null . last) l) . fmap snd . toList)
    fs

buildNextSteps :: State -> Int -> Set (Floors, Int) -> Int -> Floor -> Maybe State
buildNextSteps State {elevatorFloor = f, floors = flrs} step his floorValue el =
  let newflv = adjust (\\ el) f flrs
      newflv' = adjust (el ++) floorValue newflv
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
      nextLoad'' = if (not . null . concat . elems . takeWhileAntitone (previousFloor >=)) flrs
        then mapMaybe (buildNextSteps s step his previousFloor) nextValidLoadsSingle
        else []
   in nextLoad' ++ nextLoad''

stepZero :: Floors -> State
stepZero fls = State {elevatorFloor = 1, floors = fls}

solution1 :: State -> Int -> MVar Int -> MVar (Set (Floors, Int)) -> IO ()
solution1 st step resultMV historyMV = do
  history <- takeMVar historyMV
  _ <- putMVar historyMV (Set.insert (floors st, step) history)
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
    _    -> solution1Step st (step + 1) resultMV historyMV

solution1Step :: State -> Int -> MVar Int -> MVar (Set (Floors, Int)) -> IO ()
solution1Step st step resultMV historyMV = do
  history <- takeMVar historyMV
  --putStrLn $ show st ++ " " ++ show step-- ++ " " ++ show history
  when (step `mod` 50 == 0) $ putStr "*" >> hFlush stdout
  let nextStates = nextSteps st step history
  _ <- putMVar historyMV history
  mapM_ (\st' -> solution1 st' step resultMV historyMV) nextStates

solution1Test :: IO Bool
solution1Test = do
  let firstState = stepZero inputTest
  resultMV <- newEmptyMVar
  historyMV <- newMVar empty
  _ <- solution1 firstState 0 resultMV historyMV
  value <- takeMVar resultMV
  return $ value == 11

solution :: Floors -> IO Int
solution i = do
  let firstState = stepZero i
  resultMV <- newEmptyMVar
  historyMV <- newMVar empty
  _ <- solution1 firstState 0 resultMV historyMV
  takeMVar resultMV

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = input >>= solution

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = do
  i <- input
  let newInput = [Microchip "elerium", Microchip "dilithium", Generator "dilithium", Microchip "dilithium"]
      totalInput = adjust (newInput ++) 1 i
  solution totalInput
