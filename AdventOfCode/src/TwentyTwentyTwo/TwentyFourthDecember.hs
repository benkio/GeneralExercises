module TwentyTwentyTwo.TwentyFourthDecember where

import Data.Map (Map, empty, fromList, union, foldrWithKey)

data Blizzard = NB | SB | EB | WB deriving (Show)
data Move = NM | SM | EM | WM | W deriving (Show)
type Position = (Int, Int)
data Valley = Valley
    { entrance :: Position
    , valleyBlizzards :: Map Position Blizzard
    , valleyBounds :: Position
    , exit :: Position
    , expedition :: Position
    }
    deriving (Show)

input :: IO Valley
input = parseInput <$> readFile "input/2022/24December.txt"

valleyMove :: Valley -> Valley
valleyMove v@(Valley {valleyBlizzards = vbm, valleyBounds = vbs}) =
  v { valleyBlizzards = vbm'}
  where
    vbm' = undefined

blizzardMove :: Position -> Position -> Blizzard -> Position
blizzardMove vbounds (x, y) NB = wrap vbounds (x, y - 1)
blizzardMove vbounds (x, y) SB = wrap vbounds (x, y + 1)
blizzardMove vbounds (x, y) EB = wrap vbounds (x + 1, y)
blizzardMove vbounds (x, y) WB = wrap vbounds (x - 1, y)

wrap :: Position -> Position -> Position
wrap (vbx, vby) (x, y)
    | x < 0 = (vbx, y)
    | x > vbx = (0, y)
    | y < 0 = (x, vby)
    | y > vby = (x, 0)
    | otherwise = (x, y)

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
     in Valley
            { entrance = (s_entrance, (-1))
            , expedition = (s_entrance, (-1))
            , valleyBlizzards = s_valley
            , exit = (s_exit, length ls - 2)
            , valleyBounds = s_valleyBounds
            }

parseValley :: [String] -> Map Position Blizzard
parseValley =
    fst
        . foldl
            ( \(m, y) l ->
                ( union m $ fromList $ parseValleyRow l y
                , y + 1
                )
            )
            (empty, 0)

parseValleyRow :: String -> Int -> [(Position, Blizzard)]
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

testInput :: Valley
testInput =
    parseInput
        "#.######\n\
        \#>>.<^<#\n\
        \#.<..<<#\n\
        \#>v.><>#\n\
        \#<^v^^>#\n\
        \######.#"
