module TwentyFifteen.EighthDecember where

import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Text.Read (readMaybe)

input :: IO [String]
input = lines <$> readFile "input/2015/8December.txt"

inputTest :: String
inputTest =
    "\"\"\n\
    \\"abc\"\n\
    \\"aaa\\\"aaa\"\n\
    \\"\\x27\""

solution1 :: [String] -> Int
solution1 xs =
    let total = foldl (\acc x -> length x + acc) 0 xs
        totalEscaped = (sum . fmap countEscaped) xs
     in total - totalEscaped

countEscaped :: String -> Int
countEscaped [] = 0
countEscaped s =
    let (count, rest) = countSpecial s
     in count + countEscaped rest

countSpecial :: String -> (Int, String)
countSpecial [] = (0, [])
countSpecial x
    | "\\x" `isPrefixOf` x && length x >= 4 = (1, drop 4 x)
    | "\\\"" `isPrefixOf` x = (1, drop 2 x)
    | "\\\\" `isPrefixOf` x = (1, drop 2 x)
    | "\"" `isPrefixOf` x = (0, tail x)
    | otherwise = (1, tail x)

solution2 :: [String] -> Int
solution2 xs =
    let total = foldl (\acc x -> length x + acc) 0 xs
        totalEscaped = (foldl (\acc x -> length x + acc) 0 . fmap show) xs
     in totalEscaped - total

eighthDecemberSolution1 :: IO Int
eighthDecemberSolution1 = solution1 <$> input

eighthDecemberSolution2 :: IO Int
eighthDecemberSolution2 = solution2 <$> input
