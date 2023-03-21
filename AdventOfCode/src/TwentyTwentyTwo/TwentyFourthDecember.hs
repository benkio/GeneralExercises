module TwentyTwentyTwo.TwentyFourthDecember where

import Data.Map (Map, empty, fromList, union)

data Field = NB | SB | EB | WB deriving (Show)
data Move = NM | SM | EM | WM | W deriving (Show)
type Position = (Int, Int)
data Valley = Valley
    { entrance :: Position
    , valley :: Map Position Field
    , valleyBounds :: Position
    , exit :: Position
    , expedition :: Position
    }
    deriving (Show)

input :: IO Valley
input = parseInput <$> readFile "input/2022/24December.txt"

testInput :: Valley
testInput =
    parseInput
        "#.######\n\
        \#>>.<^<#\n\
        \#.<..<<#\n\
        \#>v.><>#\n\
        \#<^v^^>#\n\
        \######.#"

twentyFourthDecemberSolution1 :: IO Int
twentyFourthDecemberSolution1 = undefined

twentyFourthDecemberSolution2 :: IO Int
twentyFourthDecemberSolution2 = undefined

parseInput :: String -> Valley
parseInput s =
    let ls = lines s
        s_entrance = parseTopBottom $ head ls
        s_exit = parseTopBottom $ last ls
        s_valley = parseValley $ (init . tail) ls
        s_valleyBounds =
            ( ((\x -> x - 3) . length . head) ls
            , ((\x -> x - 1) . length . init . tail) ls
            )
     in Valley{entrance = (s_entrance, (-1)), expedition = (s_entrance, (-1)), valley = s_valley, exit = (s_exit, length ls - 2), valleyBounds = s_valleyBounds}

parseValley :: [String] -> Map Position Field
parseValley =
    fst
        . foldl
            ( \(m, y) l ->
                ( union m $ fromList $ parseValleyRow l y
                , y + 1
                )
            )
            (empty, 0)

parseValleyRow :: String -> Int -> [(Position, Field)]
parseValleyRow s y = go s 0
  where
    go [] x = []
    go ('#' : xs) x = go xs x
    go ('.' : xs) x = go xs (x + 1)
    go ('>' : xs) x = ((x, y), WB) : go xs (x + 1)
    go ('<' : xs) x = ((x, y), EB) : go xs (x + 1)
    go ('v' : xs) x = ((x, y), SB) : go xs (x + 1)
    go ('^' : xs) x = ((x, y), NB) : go xs (x + 1)

parseTopBottom :: String -> Int
parseTopBottom s = go s 0
  where
    go [] i = error "Entrance/exit not found"
    go ('#' : xs) i = go xs (i + 1)
    go ('.' : _) i = i - 1
