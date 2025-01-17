module TwentyTwentyFour.December25 where

import Data.Bifunctor (bimap)
import Data.List (transpose)
import Data.List.Split (splitWhen)
import Debug.Trace
import Lib.List (pairs, pairsWith)

type Keylock = [String]

input :: IO [Keylock]
input = parseInput <$> readFile "input/2024/December25.txt"

parseInput :: String -> [Keylock]
parseInput = fmap parseKeylock . splitWhen (== "") . lines -- . splitOn "\n"
  where
    parseKeylock :: [String] -> Keylock
    parseKeylock =
        -- fmap (\xs -> (break (/=head xs)) xs) .
        transpose

testInput :: [Keylock]
testInput =
    parseInput
        "#####\n\
        \.####\n\
        \.####\n\
        \.####\n\
        \.#.#.\n\
        \.#...\n\
        \.....\n\
        \\n\
        \#####\n\
        \##.##\n\
        \.#.##\n\
        \...##\n\
        \...#.\n\
        \...#.\n\
        \.....\n\
        \\n\
        \.....\n\
        \#....\n\
        \#....\n\
        \#...#\n\
        \#.#.#\n\
        \#.###\n\
        \#####\n\
        \\n\
        \.....\n\
        \.....\n\
        \#.#..\n\
        \###..\n\
        \###.#\n\
        \###.#\n\
        \#####\n\
        \\n\
        \.....\n\
        \.....\n\
        \.....\n\
        \#....\n\
        \#.#..\n\
        \#.#.#\n\
        \#####\n"

keyLockMatch :: Keylock -> Keylock -> Bool
keyLockMatch [] [] = True
keyLockMatch (k : ks) (l : ls) =
    all (\(x, y) -> (x == '.' && y == '.') || x /= y) (zip k l) && keyLockMatch ks ls

solution1 :: [Keylock] -> Int
solution1 = length . filter (uncurry keyLockMatch) . pairs

december25Solution1 :: IO Int
december25Solution1 = solution1 <$> input
