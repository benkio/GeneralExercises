module TwentySixteen.SeventhDecember where

import Data.Bifunctor
import Data.List

data IPV7 = IPV7
  { outside :: [String],
    inside :: [String]
  }
  deriving (Show)

input :: IO [IPV7]
input =
  fmap (parseIPV7 . splitBySquare) . lines
    <$> readFile "input/2016/7December.txt"

splitBySquare :: String -> [String]
splitBySquare [] = []
splitBySquare s =
  ( ( \(x, y) ->
        if null y
          then [x]
          else
            ( (\(x', y') -> [x, x'] ++ splitBySquare (tail y'))
                . break (']' ==)
                . tail
            )
              y
    )
      . break ('[' ==)
  )
    s

parseIPV7 :: [String] -> IPV7
parseIPV7 =
  (\(out, in') -> IPV7 {outside = out, inside = in'})
    . bimap (fmap fst) (fmap fst)
    . partition (even . snd)
    . (`zip` [0 ..])

abbaCheck :: String -> Bool
abbaCheck [] = False
abbaCheck s = abbaCheck' (take 4 s) || abbaCheck (tail s)

abbaCheck' :: String -> Bool
abbaCheck' s =
  length s == 4 && (take 2 s == take 2 (reverse s)) && head s /= s !! 1

isValidAbba :: IPV7 -> Bool
isValidAbba IPV7 {outside = o, inside = i} =
  or (fmap abbaCheck o) && and (fmap (not . abbaCheck) i)

solution1 :: [IPV7] -> Int
solution1 = length . filter isValidAbba

inputTest :: [IPV7]
inputTest =
  (fmap (parseIPV7 . splitBySquare) . lines)
    "abba[mnop]qrst\n\
    \abcd[bddb]xyyx\n\
    \aaaa[qwer]tyui\n\
    \ioxxoj[asdfgh]zxcvbn"

solution1Test :: Bool
solution1Test = solution1 inputTest2 == 2

seventhDecemberSolution1 :: IO Int
seventhDecemberSolution1 = solution1 <$> input

abaFind :: String -> (String -> String) -> [String]
abaFind [] _ = []
abaFind s check = check (take 3 s) : abaFind (tail s) check

invertAba :: String -> String
invertAba [a, b, _] = [b, a, b]
invertAba _ = ""

abaCheck :: String -> String
abaCheck x
  | length x == 3 && head x == last x && head x /= x !! 1 = x
  | otherwise = ""

isValidAba :: IPV7 -> Bool
isValidAba IPV7 {outside = o, inside = i} =
  any ((`elem` abaOutside) . invertAba) abaInside
  where
    abaInside = (filter (not . null) . concatMap (`abaFind` abaCheck)) i
    abaOutside = (filter (not . null) . concatMap (`abaFind` abaCheck)) o

inputTest2 :: [IPV7]
inputTest2 =
  (fmap (parseIPV7 . splitBySquare) . lines)
    "aba[bab]xyz\n\
    \xyx[xyx]xyx\n\
    \aaa[kek]eke\n\
    \zazbz[bzb]cdb"

solution2Test :: Bool
solution2Test = solution2 inputTest2 == 3

solution2 :: [IPV7] -> Int
solution2 = length . filter isValidAba

seventhDecemberSolution2 :: IO Int
seventhDecemberSolution2 = solution2 <$> input
