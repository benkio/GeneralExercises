{-# LANGUAGE OverloadedStrings #-}

module TwentySixteen.EleventhDecember where

import           Control.Concurrent
import           Data.List
import           Data.Map           (Map, adjust, elems, fromList,
                                     takeWhileAntitone, toList)
import qualified Data.Map           as Map (lookup)
import           Data.Maybe
import           Data.Ord
import           Data.Set           (Set, empty)
import qualified Data.Set           as Set (insert)

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
floorsEquality :: Floors -> Floors -> Bool
floorsEquality fs1 fs2 = all (null . uncurry (\\)) $ elems fs1 `zip` elems fs2

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

validLoads :: Floor -> [Floor]
validLoads fls =
  (mapMaybe
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

nextSteps :: State -> Set Floors -> [State]
nextSteps State {elevatorFloor = f, floors = flrs} his =
  let (previousFloor, currentFloor, nextFloor) =
        (max (f - 1) 1, fromJust (Map.lookup f flrs), min (f + 1) 4)
      nextValidLoads =
        (sortOn (Data.Ord.Down . length) . validLoads) currentFloor
      buildNextSteps =
        \floorValue el ->
          let newflv = adjust (\\ el) f flrs
              newflv' = adjust (el ++) floorValue newflv
           in if (not . (\x -> any (`floorsEquality` x) his)) newflv'
                then Just State {elevatorFloor = floorValue, floors = newflv'}
                else Nothing
      nextLoadUp =
        (mapMaybe (buildNextSteps nextFloor) .
         filter (\el -> nextFloor /= f && validateFloor nextFloor el flrs))
          nextValidLoads
      nextLoadDown =
        (mapMaybe (buildNextSteps previousFloor) .
         filter
           (\el ->
              previousFloor /= f &&
              (not .
               null . concat . elems . takeWhileAntitone (previousFloor >=))
                flrs &&
              validateFloor previousFloor el flrs) .
         reverse)
          nextValidLoads
   in if null nextLoadUp
        then nextLoadDown
        else nextLoadUp

stepZero :: Floors -> State
stepZero fls = State {elevatorFloor = 1, floors = fls}

solution1 :: State -> Int -> MVar Int -> MVar (Set Floors) -> IO ()
solution1 st step resultMV historyMV = do
  history <- takeMVar historyMV
  _ <- putMVar historyMV (Set.insert (floors st) history)
  resultNotFound <- isEmptyMVar resultMV
  --putStrLn $ show st ++ " - resultNotFound " ++ show resultNotFound
  case (not resultNotFound, endCondition st) of
    (_, True) -> do
      previousResult <- tryTakeMVar resultMV
      putStrLn $ "found a Result: " ++ show step
      if isJust previousResult then putMVar resultMV (min step (fromJust previousResult)) else putMVar resultMV step
    (True, _) -> do
      result <- takeMVar resultMV
      let isStepHigher = step >= result
      --putStr $ show step ++ " "
      _ <- putMVar resultMV result
      if isStepHigher then return () else solution1Step st (step + 1) resultMV historyMV
    _    -> solution1Step st (step + 1) resultMV historyMV

solution1Step :: State -> Int -> MVar Int -> MVar (Set Floors) -> IO ()
solution1Step st step resultMV historyMV = do
  history <- takeMVar historyMV
  let nextStates = nextSteps st history
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

eleventhDecemberSolution1 :: IO Int
eleventhDecemberSolution1 = do
  i <- input
  let firstState = stepZero i
  resultMV <- newEmptyMVar
  historyMV <- newMVar empty
  _ <- solution1 firstState 0 resultMV historyMV
  takeMVar resultMV

solution2 :: String -> Int
solution2 = undefined

eleventhDecemberSolution2 :: IO Int
eleventhDecemberSolution2 = undefined
