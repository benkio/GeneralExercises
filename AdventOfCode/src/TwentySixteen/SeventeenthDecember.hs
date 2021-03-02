module TwentySixteen.SeventeenthDecember where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Digest.Pure.MD5       as M
import           Data.List

data Door = U | D | L | R deriving (Show, Eq, Ord)
type Coordinate = (Int, Int)

input :: IO String
input = readFile "input/2016/17December.txt"

generateMD5 :: String -> String
generateMD5 = show . M.md5 . B.pack

gridMaxCoordinate :: Coordinate
gridMaxCoordinate = (4,4)

grid :: [Coordinate]
grid = [(x, y) | x <- [0..fst gridMaxCoordinate], y <- [0..snd gridMaxCoordinate]]

availableDoors :: Coordinate -> [Door]
availableDoors (x,y) = avaliableDoorsByX x ++ avaliableDoorsByY y

avaliableDoorsByX :: Int -> [Door]
avaliableDoorsByX x
  | x == 0 = [R]
  | x == fst gridMaxCoordinate = [L]
  | otherwise = [R, L]

avaliableDoorsByY :: Int -> [Door]
avaliableDoorsByY y
  | y == 0 = [D]
  | y == snd gridMaxCoordinate = [U]
  | otherwise = [D, U]

isOpen :: Char -> Bool
isOpen = (`elem` ['b'..'f'])

openDoors :: Coordinate -> [Door] -> String -> [Door]
openDoors pos moves hash = (filter (`elem` availableDoors pos) . fmap snd . filter fst . (`zip` [U,D,L,R]) . fmap isOpen . take 4 . generateMD5 . (hash ++ ) . concatMap show) moves

nextCoordinate :: Coordinate -> Door -> Coordinate
nextCoordinate (x,y) U = (x,y-1)
nextCoordinate (x,y) D = (x,y+1)
nextCoordinate (x,y) L = (x-1,y)
nextCoordinate (x,y) R = (x+1,y)

solution1 :: Coordinate -> [Door] -> String -> [Door]
solution1 pos moves hash
  | pos == gridMaxCoordinate = moves
  | null nextMovements = []
  | otherwise =
    let x = (filter (not . null) .fmap (\d -> solution1 (nextCoordinate pos d) (moves ++ [d]) hash)) nextMovements
    in if null x then [] else minimumBy (\a b -> compare (length a) (length b)) x
    where nextMovements = openDoors pos moves hash

testSolution1 :: Bool
testSolution1 = solution1 (0,0) [] "ihgpwlah" == [D,D,R,R,R,D]

seventeenthDecemberSolution1 :: IO Int
seventeenthDecemberSolution1 = undefined

solution2 :: String -> Int
solution2 = undefined

seventeenthDecemberSolution2 :: IO Int
seventeenthDecemberSolution2 = undefined
