module TwentyTwentyFour.December13 where

import Data.Maybe (mapMaybe)

import Control.Monad (guard)
import Data.List.Split (splitOn)
import GHC.Float (int2Double)
import GHC.Float.RealFracMethods (properFractionDoubleInt)
import Lib.Coord (Coord, cardinalNeighboors, findCardinalNeighboors)

data Arcade = A
    { btnA :: (Int, Int)
    , btnB :: (Int, Int)
    , prize :: Coord
    }
    deriving (Show)

input :: IO [Arcade]
input = parseInput <$> readFile "input/2024/December13.txt"

parseInput :: String -> [Arcade]
parseInput = fmap (parseArcade . splitOn "\n") . splitOn "\n\n"
  where
    dropTillPlus = tail . dropWhile (/= '+')
    dropTillComma = tail . dropWhile (/= ',')
    dropTillEqual = tail . dropWhile (/= '=')
    parseButton s = (((\x -> read x :: Int) . takeWhile (/= ',') . dropTillPlus) s, ((\x -> read x :: Int) . takeWhile (/= '\n') . dropTillPlus . dropTillComma) s)
    parsePrize s = (((\x -> read x :: Int) . takeWhile (/= ',') . dropTillEqual) s, ((\x -> read x :: Int) . takeWhile (/= '\n') . dropTillEqual . dropTillComma) s)
    parseArcade :: [String] -> Arcade
    parseArcade xs =
        A
            { btnA = parseButton (head xs)
            , btnB = parseButton (xs !! 1)
            , prize = parsePrize (xs !! 2)
            }

{-
a * x1 + b * x2 = c
x2 = (c - a * x1)/b
-}
deriveBtnBCoefficientX :: Int -> Int -> Int -> Int -> Maybe Int
deriveBtnBCoefficientX a x1 b c =
    if decimals /= 0.0 then Nothing else Just result
  where
    x2 = int2Double (c - a * x1) / (fromIntegral b)
    (result, decimals) = properFractionDoubleInt x2

findBntCoefficients :: Arcade -> [(Int, Int)]
findBntCoefficients A{btnA = (btnAx, btnAy), btnB = (btnBx, btnBy), prize = (pX, pY)} =
    mapMaybe search [0 .. 100]
  where
    search coeffBtnA = do
        coeffBtnB <- deriveBtnBCoefficientX btnAx coeffBtnA btnBx pX
        guard $ (btnAy * coeffBtnA) + (btnBy * coeffBtnB) == pY
        return (coeffBtnA, coeffBtnB)

calcTokens :: (Int, Int) -> Int
calcTokens (coeffBtnA, coeffBtnB) = coeffBtnA * 3 + coeffBtnB

testInput :: [Arcade]
testInput =
    parseInput
        "Button A: X+94, Y+34\n\
        \Button B: X+22, Y+67\n\
        \Prize: X=8400, Y=5400\n\
        \\n\
        \Button A: X+26, Y+66\n\
        \Button B: X+67, Y+21\n\
        \Prize: X=12748, Y=12176\n\
        \\n\
        \Button A: X+17, Y+86\n\
        \Button B: X+84, Y+37\n\
        \Prize: X=7870, Y=6450\n\
        \\n\
        \Button A: X+69, Y+23\n\
        \Button B: X+27, Y+71\n\
        \Prize: X=18641, Y=10279\n"

solution1 :: [Arcade] -> Int
solution1 = sum . concatMap (fmap calcTokens . findBntCoefficients)

december13Solution1 :: IO Int
december13Solution1 = solution1 <$> input

solution2 :: [Arcade] -> Int
solution2 = undefined

december13Solution2 :: IO Int
december13Solution2 = solution2 <$> input
