{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December13 where

import Control.Monad (guard)
import Data.Bifunctor (bimap, first, second)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Lib.Coord (Coord, cardinalNeighboors)
import Lib.CoordMap (findCardinalNeighboors)
import Lib.Math (isInteger, twoLinearEqSolver)

data Arcade = A
    { btnA :: (Int, Int)
    , btnB :: (Int, Int)
    , prize :: (Int, Int)
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

findBntCoefficients' :: Arcade -> Maybe (Int, Int)
findBntCoefficients' A{btnA = (btnAx, btnAy), btnB = (btnBx, btnBy), prize = (pX, pY)} = twoLinearEqSolver btnAx btnBx pX btnAy btnBy pY

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
solution1 =
    sum
        . fmap (calcTokens . snd)
        . filter
            ( \(arcade, (a, b)) ->
                a <= 100
                    && b <= 100
                    && checkSolution arcade (a, b)
            )
        . mapMaybe (\a -> (a,) <$> findBntCoefficients' a)

december13Solution1 :: IO Int
december13Solution1 = solution1 <$> input

checkSolution :: Arcade -> (Int, Int) -> Bool
checkSolution A{btnA = (btnAx, btnAy), btnB = (btnBx, btnBy), prize = (pX, pY)} (cA, cB) =
    (btnAx * cA) + (btnBx * cB) == pX
        && (btnAy * cA) + (btnBy * cB) == pY

solution2 :: [Arcade] -> Int
solution2 =
    sum
        . fmap (calcTokens . snd)
        . filter (uncurry checkSolution)
        . mapMaybe
            ( \a ->
                let a' = a{prize = bimap increasePrize increasePrize (prize a)}
                 in (a',) <$> findBntCoefficients' a'
            )
  where
    increasePrize = (+) 10000000000000

-- too high 133834706986425
-- too low  71037763783856
december13Solution2 :: IO Int
december13Solution2 = solution2 <$> input
