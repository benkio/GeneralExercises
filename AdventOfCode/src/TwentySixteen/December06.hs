module TwentySixteen.December06 where

import Data.List

input :: IO String
input = readFile "input/2016/6December.txt"

inputTest :: String
inputTest =
    "eedadn\n\
    \drvtee\n\
    \eandsr\n\
    \raavrd\n\
    \atevrs\n\
    \tsrnev\n\
    \sdttsa\n\
    \rasrtv\n\
    \nssdts\n\
    \ntnada\n\
    \svetve\n\
    \tesnvt\n\
    \vntsnd\n\
    \vrdear\n\
    \dvrsen\n\
    \enarar"

solution ::
    String -> ((String -> String -> Ordering) -> [String] -> String) -> String
solution z f =
    ( fmap (head . f (\x y -> length x `compare` length y) . group . sort)
        . transpose
        . lines
    )
        z

solutionTest :: Bool
solutionTest = solution inputTest maximumBy == "easter"

december06Solution1 :: IO String
december06Solution1 = (`solution` maximumBy) <$> input

december06Solution2 :: IO String
december06Solution2 = (`solution` minimumBy) <$> input
