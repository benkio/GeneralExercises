module TwentyTwentyTwo.December25 where

import Data.List (find)

input :: IO [String]
input = lines <$> readFile "input/2022/25December.txt"

testInput :: [String]
testInput =
    lines
        "1=-0-2\n\
        \12111\n\
        \2=0=\n\
        \21\n\
        \2=01\n\
        \111\n\
        \20012\n\
        \112\n\
        \1=-1=\n\
        \1-12\n\
        \12\n\
        \1=\n\
        \122"

snafuToNum :: String -> Int
snafuToNum = sum . fmap (\(p, x) -> p * singleSnafuToNum x) . zip power5 . reverse

power5 = fmap (5 ^) [0 ..]

numToSnafu :: Int -> Int -> String
numToSnafu i x = if i' == 0 then [c] else c : numToSnafu (i' - 1) r
  where
    (c, r, i') = powerToSinafu (i, x)

powerToSinafu :: (Int, Int) -> (Char, Int, Int)
powerToSinafu (i, x)
    | d > 2 = powerToSinafu (i + 1, x)
    | (d == 0 && m >= (p - hp)) || (d == 1 && m <= hp) = (adjustSignum '1', x + (((* (-1)) . signum) x * p), i)
    | (d == 1 && m >= (p - hp)) || (d == 2 && m <= hp) = (adjustSignum '2', x + (((* (-1)) . signum) x * p * 2), i)
    | otherwise = ('0', x, i)
  where
    p = power5 !! i
    d = abs x `div` p
    m = abs x `mod` p
    hp = p `div` 2
    adjustSignum '1' = if signum x < 0 then '-' else '1'
    adjustSignum '2' = if signum x < 0 then '=' else '2'

solution :: [String] -> String
solution = numToSnafu 0 . sum . fmap snafuToNum

twentyFifthDecemberSolution1 :: IO String
twentyFifthDecemberSolution1 = solution <$> input

singleSnafuToNum '2' = 2
singleSnafuToNum '1' = 1
singleSnafuToNum '0' = 0
singleSnafuToNum '-' = -1
singleSnafuToNum '=' = -2

singleNumToSnafu 2 = '2'
singleNumToSnafu 1 = '1'
singleNumToSnafu 0 = '0'
singleNumToSnafu (-1) = '-'
singleNumToSnafu (-2) = '='
singleNumToSnafu x = error $ "Invalid input: " ++ show x
