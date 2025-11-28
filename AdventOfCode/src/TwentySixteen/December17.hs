module TwentySixteen.December17 where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor
import Data.List
import Data.Maybe
import Lib.MD5 (generateMD5)

data Door = U | D | L | R deriving (Show, Eq, Ord)

type Coordinate = (Int, Int)

input :: IO String
input = readFile "input/2016/17December.txt"

gridMaxCoordinate :: Coordinate
gridMaxCoordinate = (3, 3)

availableDoors :: Coordinate -> [Door]
availableDoors (x, y) = avaliableDoorsByX x ++ avaliableDoorsByY y

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
isOpen = (`elem` ['b' .. 'f'])

openDoors :: Coordinate -> [Door] -> String -> [Door]
openDoors pos moves hash =
    ( filter (`elem` availableDoors pos)
        . fmap snd
        . filter fst
        . (`zip` [U, D, L, R])
        . fmap isOpen
        . take 4
        . generateMD5
        . (hash ++)
        . concatMap show
    )
        moves

nextCoordinate :: Coordinate -> Door -> Coordinate
nextCoordinate (x, y) U = (x, y - 1)
nextCoordinate (x, y) D = (x, y + 1)
nextCoordinate (x, y) L = (x - 1, y)
nextCoordinate (x, y) R = (x + 1, y)

solutionStep :: Coordinate -> [Door] -> String -> IO [(Coordinate, [Door])]
solutionStep pos moves hash = do
    let nextMovements = openDoors pos moves hash
    if null nextMovements
        then return []
        else return (fmap (\d -> (nextCoordinate pos d, moves ++ [d])) nextMovements)

solutionShortest :: [(Coordinate, [Door])] -> String -> IO [Door]
solutionShortest [] _ = return []
solutionShortest poss hash
    | any ((== gridMaxCoordinate) . fst) poss =
        ( return
            . snd
            . fromJust
            . find ((== gridMaxCoordinate) . fst)
        )
            poss
    | otherwise = do
        nMoves <- traverse (\(pos, moves) -> solutionStep pos moves hash) poss
        solutionShortest (concat nMoves) hash

testSolution1 :: IO Bool
testSolution1 = do
    test1 <- solutionShortest [((0, 0), [])] "ihgpwlah" <&> (== [D, D, R, R, R, D])
    test2 <- solutionShortest [((0, 0), [])] "kglvqrro" <&> (== [D, D, U, D, R, L, R, R, U, D, R, D])
    test3 <- solutionShortest [((0, 0), [])] "ulqzkmiv" <&> (== [D, R, U, R, D, R, U, D, D, L, L, D, L, U, U, R, R, D, U, L, R, L, D, U, U, D, D, D, R, R])
    return $ test1 && test2 && test3

december17Solution1 :: IO String
december17Solution1 = do
    i <- input
    ds <- solutionShortest [((0, 0), [])] i
    return $ concatMap show ds

solutionLongest :: [(Coordinate, [Door])] -> [Door] -> String -> IO [Door]
solutionLongest [] longest _ = return longest
solutionLongest poss longest hash
    | any ((== gridMaxCoordinate) . fst) poss = do
        let terminated = filter ((== gridMaxCoordinate) . fst) poss
            longest' =
                ( maximumBy (\d d' -> compare (length d) (length d'))
                    . (\dss -> dss ++ [longest])
                    . fmap snd
                )
                    terminated
        solutionLongest (poss \\ terminated) longest' hash
    | otherwise = do
        nMoves <- traverse (\(pos, moves) -> solutionStep pos moves hash) poss
        solutionLongest (concat nMoves) longest hash

testSolution2 :: IO Bool
testSolution2 = do
    test1 <- solutionLongest [((0, 0), [])] [] "ihgpwlah" <&> ((== 370) . length)
    test2 <- solutionLongest [((0, 0), [])] [] "kglvqrro" <&> ((== 492) . length)
    test3 <- solutionLongest [((0, 0), [])] [] "ulqzkmiv" <&> ((== 830) . length)
    return $ test1 && test2 && test3

december17Solution2 :: IO Int
december17Solution2 = do
    i <- input
    ds <- solutionLongest [((0, 0), [])] [] i
    return $ length ds
