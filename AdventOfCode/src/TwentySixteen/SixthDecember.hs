module TwentySixteen.SixthDecember where

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

sixthDecemberSolution1 :: IO String
sixthDecemberSolution1 = (`solution` maximumBy) <$> input

sixthDecemberSolution2 :: IO String
sixthDecemberSolution2 = (`solution` minimumBy) <$> input
