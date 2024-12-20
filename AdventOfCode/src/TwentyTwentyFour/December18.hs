module TwentyTwentyFour.December18 where

import Debug.Trace (traceShow, traceShowId)

import Data.Map (Map, fromList)
import Data.Maybe (mapMaybe)
import Lib.Coord (Coord, coordDistance)
import Lib.CoordMap (findBranches)
import Lib.Direction (Direction (..))
import Lib.List (find')
import Lib.Parse (parseCoords)
import Lib.Pathfinding (Node (..), mapToPaths, minimumSteps)
import Lib.Print (printGridMap)
import Text.Printf (printf)

input :: IO [Coord]
input = parseInput <$> readFile "input/2024/December18.txt"

inputRange :: Int
inputRange = 70
solution1ByteAmount :: Int
solution1ByteAmount = 1024
startPosition :: Coord
startPosition = (0, 0)
endPosition :: Int -> Coord
endPosition range = (range, range)
start = (startPosition, 'S')
end range = (endPosition range, 'E')

makeMap :: Int -> Int -> [Coord] -> Map Coord Char
makeMap range byteAmount xs =
    fromList
        . (++ [start, end range])
        . mapMaybe
            ( \c ->
                if c `elem` take byteAmount xs then Nothing else Just (c, '.')
            )
        $ [(x, y) | x <- [0 .. range], y <- [0 .. range]]

isTheEnd :: Int -> Coord -> Char -> Bool
isTheEnd range c v = c == endPosition range && v == 'E'

paths :: Int -> Map Coord Char -> [[Node Char]]
paths range = mapToPaths start East (isTheEnd range) (calculateScore range) filterNodeF keepNextNodeByScoreF (sortNodesF range)

calculateScore :: Int -> Node Char -> Int
calculateScore range n = uncurry (+) (coordDistance (nc n) (fst (end range)))
filterNodeF, keepNextNodeByScoreF :: Int -> Int -> Bool
filterNodeF curr past = curr >= (past + stepsAwayFilter)
keepNextNodeByScoreF next past = next <= (past + stepsAwayFilter)
sortNodesF :: Int -> (Node Char, Int) -> Int
sortNodesF range (_, score) = score * 4

stepsAwayFilter :: Int
stepsAwayFilter = 0

solution1 :: Int -> Int -> [Coord] -> Int
solution1 range byteAmount = fst . minimumSteps . paths range . makeMap range byteAmount

december18Solution1 :: IO Int
december18Solution1 = solution1 inputRange solution1ByteAmount <$> input

maps :: Int -> Int -> [Coord] -> [(Coord, Map Coord Char)]
maps range byteAmount ms = fmap (\ba -> (ms !! (ba - 1), makeMap range ba ms)) [byteAmount .. length ms]

test =
    find' (\(_, ms) -> null (paths testRange ms)) $
        maps testRange testSolution1ByteAmount testInput

solution2 :: Int -> Int -> [Coord] -> String
solution2 range byteAmount =
    ( (\(a, b) -> printf "%s,%s" (show a) (show b))
        . fst
    )
        . mergeSearch range
        . maps range byteAmount

mergeSearch :: Int -> [(Coord, Map Coord Char)] -> (Coord, Map Coord Char)
mergeSearch range cs
    | length cs == 1 = head cs
    | length cs == 2 && null result = c
    | length cs == 2 && not (null result) = last cs
    | null result = mergeSearch range lcs
    | otherwise = mergeSearch range rcs
  where
    middleIndex = length cs `div` 2
    c = cs !! middleIndex
    (lcs, rcs) = splitAt middleIndex cs
    result = paths range (snd c)

december18Solution2 :: IO String
december18Solution2 = solution2 inputRange solution1ByteAmount <$> input

parseInput :: String -> [Coord]
parseInput = parseCoords

testRange :: Int
testRange = 6
testSolution1ByteAmount :: Int
testSolution1ByteAmount = 12
testInput :: [Coord]
testInput =
    parseInput
        "5,4\n\
        \4,2\n\
        \4,5\n\
        \3,0\n\
        \2,1\n\
        \6,3\n\
        \2,4\n\
        \1,5\n\
        \0,6\n\
        \3,3\n\
        \2,6\n\
        \5,1\n\
        \1,2\n\
        \5,5\n\
        \2,5\n\
        \6,5\n\
        \1,4\n\
        \0,4\n\
        \6,4\n\
        \1,1\n\
        \6,1\n\
        \1,0\n\
        \0,5\n\
        \1,6\n\
        \2,0\n"

testInput' :: [Coord]
testInput' =
    parseInput
        "5,4\n\
        \4,2\n\
        \4,5\n\
        \3,0\n\
        \2,1\n\
        \6,3\n\
        \2,4\n\
        \1,5\n\
        \0,6\n\
        \3,3\n\
        \2,6\n\
        \5,1\n\
        \1,2\n\
        \5,5\n\
        \2,5\n\
        \6,5\n\
        \1,4\n\
        \0,4\n\
        \6,4\n\
        \1,1\n\
        \6,1\n\
        \1,0\n\
        \0,5\n\
        \1,6\n\
        \2,0\n"

printMemory :: Int -> Int -> [Coord] -> IO ()
printMemory range byteAmount = putStrLn . printGridMap (\_ ma -> maybe "#" (: []) ma) . makeMap range byteAmount
