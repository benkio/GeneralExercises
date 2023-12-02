module TwentyTwentyThree.SecondDecember where

import Data.Char
import Data.List.Split
import Debug.Trace

data Color = Blue | Green | Red deriving (Show)
type ExtractionGroup = [Extraction]
type Extraction = [ExtractionSingle]
data ExtractionSingle = ExtractionSingle {value :: Int, color :: Color}
data Game = Game {gid :: Int, extractionGroup :: ExtractionGroup} deriving (Show)
data Bag = Bag {blue :: Int, red :: Int, green :: Int} deriving (Show)

instance Show ExtractionSingle where
    show (ExtractionSingle{value = v, color = c}) = show v ++ " " ++ show c

input :: IO [Game]
input = fmap parseInput . lines <$> readFile "input/2023/2December.txt"

parseInput :: String -> Game
parseInput i = Game{gid = gameId, extractionGroup = extractionGroup}
  where
    parseEstraction :: String -> (ExtractionSingle, String)
    parseEstraction s =
        ( \x ->
            ( ExtractionSingle{value = read ((head . words) x) :: Int, color = parseColor ((last . words) x)}
            , (drop 2 . dropWhile (/= ',')) s
            )
        )
            $ takeWhile (/= ',') s
    parseExtractions :: String -> [ExtractionSingle]
    parseExtractions [] = []
    parseExtractions s = ((\(e, s) -> e : parseExtractions s) . parseEstraction) s
    parseColor :: String -> Color
    parseColor "blue" = Blue
    parseColor "red" = Red
    parseColor "green" = Green
    parseExtractionGroup :: String -> [String]
    parseExtractionGroup = splitOn ";"
    parseGameId :: String -> (Int, String)
    parseGameId s = (((\x -> (read x :: Int)) . takeWhile (/= ':') . tail . dropWhile (/= ' ')) s, (drop 2 . dropWhile (/= ':')) s)
    (gameId, i') = parseGameId i
    extractionGroup = (fmap parseExtractions . parseExtractionGroup) i'

testInput :: [Game]
testInput =
    (fmap parseInput . lines)
        "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
        \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
        \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
        \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
        \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

bag :: Bag
bag = Bag{red = 12, green = 13, blue = 14}

emptyBag :: Bag
emptyBag = Bag{red = 0, green = 0, blue = 0}

extractionToBag :: Extraction -> Bag
extractionToBag = foldl addToBag emptyBag
  where
    addToBag b (ExtractionSingle{value = v, color = c}) = case c of
        Blue -> b{blue = blue b + v}
        Green -> b{green = green b + v}
        Red -> b{red = red b + v}

isInBag :: Extraction -> Bool
isInBag = checkBag bag . extractionToBag
  where
    checkBag (Bag{blue = b, green = g, red = r}) (Bag{blue = b', green = g', red = r'}) =
        b' <= b
            && g' <= g
            && r' <= r

isGameInBag :: Game -> Bool
isGameInBag = all isInBag . extractionGroup

solution1 :: [Game] -> Int
solution1 = sum . fmap gid . filter isGameInBag

secondDecemberSolution1 :: IO Int
secondDecemberSolution1 = solution1 <$> input

minimumBag :: Game -> Bag
minimumBag g =
    ( foldl minMaxBag ((extractionToBag . head . extractionGroup) g)
        . fmap extractionToBag
        . tail
        . extractionGroup
    )
        g
  where
    minMaxBag :: Bag -> Bag -> Bag
    minMaxBag (Bag{blue = b, green = g, red = r}) (Bag{blue = b', green = g', red = r'}) =
        Bag{blue = nonZeroMax b b', green = nonZeroMax g' g, red = nonZeroMax r' r}
    nonZeroMax :: Int -> Int -> Int
    nonZeroMax 0 y = y
    nonZeroMax x 0 = x
    nonZeroMax x y = max x y

powerSetBag :: Bag -> Int
powerSetBag Bag{blue = b, green = g, red = r} = b * r * g

solution2 :: [Game] -> Int
solution2 = sum . map (powerSetBag . minimumBag)

secondDecemberSolution2 :: IO Int
secondDecemberSolution2 = solution2 <$> input
