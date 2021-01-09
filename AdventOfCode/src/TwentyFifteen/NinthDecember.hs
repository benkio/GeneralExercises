module TwentyFifteen.NinthDecember where

import           Data.List
import           Data.Map   (Map)
import qualified Data.Map   as Map
import           Data.Maybe

data Travel = Travel String String Int deriving (Show, Eq)

getDestinations :: [Travel] -> [String]
getDestinations []                  = []
getDestinations [Travel _ x _]      = [x]
getDestinations ((Travel x y _):xs) = x:y:getDestinations xs

getDestination :: Travel -> String
getDestination (Travel _ d _) = d
getSource :: Travel -> String
getSource (Travel s _ _) = s
getTime :: Travel -> Int
getTime (Travel _ _ t) = t

input :: IO [Travel]
input = fmap parseTravel . lines <$> readFile "input/2015/9December.txt"

parseTravel :: String -> Travel
parseTravel s =
  let ws = words s
      source = head ws
      dest = ws !! 2
      time = read (last ws) :: Int
  in Travel source dest time

travelsToMap :: [Travel] -> Map (String, String) Int
travelsToMap = foldl insertTravel Map.empty
  where insertTravel :: Map (String, String) Int -> Travel -> Map (String, String) Int
        insertTravel m (Travel s d x) =
          let m' = Map.insert (s, d) x m
          in Map.insert (d, s) x m'

allDestinations :: Map (String, String) Int -> [String]
allDestinations = nub . fmap fst . Map.keys

travelValue :: Map (String,String) Int -> [(String, String)] -> Int
travelValue m  = foldl (\acc x -> acc + fromJust (Map.lookup x m)) 0

findRoute :: (Int -> Int -> Int) -> Map (String,String) Int -> Int
findRoute compare_ m =
  let allDestination = allDestinations m
      allTravels = (fmap (\l -> l `zip` tail l) . permutations) allDestination
      firstTravel = travelValue m (head allTravels)
  in foldl' (\x t -> compare_ (travelValue m t) x) firstTravel (tail allTravels)

inputTest :: [Travel]
inputTest = (fmap parseTravel . lines) "London to Dublin = 464\n\
\London to Belfast = 518\n\
\Dublin to Belfast = 141"

solution1Test :: Bool
solution1Test = ((== 605) . findRoute min . travelsToMap) inputTest

solution2Test :: Bool
solution2Test = ((== 982) . findRoute max . travelsToMap) inputTest

ninthDecemberSolution1 :: IO Int
ninthDecemberSolution1 = findRoute min . travelsToMap <$> input

ninthDecemberSolution2 :: IO Int
ninthDecemberSolution2 = findRoute max . travelsToMap <$> input
