module TwentyTwentyTwo.TwentyFifthDecember where

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
  where
    power5 = fmap (\p -> 5 ^ p) [0 ..]

test = sum $ fmap snafuToNum testInput

twentyFifthDecemberSolution1 :: IO Int
twentyFifthDecemberSolution1 = undefined

twentyFifthDecemberSolution2 :: IO Int
twentyFifthDecemberSolution2 = undefined

singleSnafuToNum '2' = 2
singleSnafuToNum '1' = 1
singleSnafuToNum '0' = 0
singleSnafuToNum '-' = (-1)
singleSnafuToNum '=' = (-2)

singleNumToSnafu 2 = '2'
singleNumToSnafu 1 = '1'
singleNumToSnafu 0 = '0'
singleNumToSnafu (-1) = '-'
singleNumToSnafu (-2) = '='
