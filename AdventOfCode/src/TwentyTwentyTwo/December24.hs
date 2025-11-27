module TwentyTwentyTwo.December24 where

import Data.Bifunctor (first)
import Data.List (find, maximumBy, nubBy)
import Data.Maybe (fromJust)
import Text.Printf

data Blizzard = NB Position | SB Position | EB Position | WB Position deriving (Show, Eq)

data Move = NM | SM | EM | WM | W deriving (Show)

type Position = (Int, Int)

data Valley = Valley
    { entrance :: Position
    , valleyBlizzards :: [Blizzard]
    , valleyBounds :: Position
    , exit :: Position
    }
    deriving (Show)

-- This indetifies a configuration fully, the minute is the index of the valleys in the env
-- Eq intance. State with the same position and minute % valleys should be the same!!!
data State = State {currPos :: Position, minute :: Int} deriving (Show)

-- Used when generating the tree of states in search of the goal
data Env = Env {valleys :: [[Blizzard]], target :: Position, start :: Position, vBounds :: Position}

input :: IO Valley
input = parseInput <$> readFile "input/2022/24December.txt"

searchTillTargetFromValley :: Valley -> ([State], Env)
searchTillTargetFromValley v = searchTillTarget [s] e
  where
    (s, e) = valleyToSearchInit v

searchTillTarget :: [State] -> Env -> ([State], Env)
searchTillTarget s e = search s e exitCondition
  where
    exitCondition = any ((== target e) . currPos)

getResult :: [State] -> Env -> State
getResult s e = (fromJust . find ((== target e) . currPos)) s

search :: [State] -> Env -> ([State] -> Bool) -> ([State], Env)
search ss e f
    | f ss = (ss, e)
    | otherwise = search ns e f
  where
    ns = nubBy (eqState ((length . valleys) e)) $ concatMap (`nextStates` e) ss

-- Generate the next states by generating the next possible positions given the current blizzard
nextStates :: State -> Env -> [State]
nextStates s@(State{currPos = crp, minute = m}) (Env{valleys = vss, target = ext, start = str, vBounds = vbs}) = ns
  where
    bl = vss !! ((m + 1) `mod` length vss)
    ns = expeditionMove s bl vbs ext str

-- Generate new states based on where expedition can go, or just wait where it is
-- The Blizzard in input should be the next configuration since the expedition and the blizzards act simultaneously
expeditionMove :: State -> [Blizzard] -> Position -> Position -> Position -> [State]
expeditionMove (State{currPos = (x, y), minute = m}) bs (vbsx, vbsy) entrance exit =
    fmap (\p -> State{currPos = p, minute = m + 1}) freePos
  where
    notOutOfBounds (x, y) =
        (x >= 0 && y >= 0 && x <= vbsx && y <= vbsy) || (x, y) == entrance || (x, y) == exit
    ps = filter notOutOfBounds [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    ns = filter ((`elem` ps) . blizzardPos) bs
    freePos = filter (`notElem` fmap blizzardPos ns) ps

eqState :: Int -> State -> State -> Bool
eqState valleyNum (State{currPos = cr1, minute = m1}) (State{currPos = cr2, minute = m2}) =
    cr1 == cr2 && m1 `mod` valleyNum == m2 `mod` valleyNum

valleyToSearchInit :: Valley -> (State, Env)
valleyToSearchInit v@(Valley{entrance = ent, valleyBounds = vbs, exit = ext}) =
    ( State{currPos = ent, minute = 0}
    , Env{valleys = allValleys v, target = ext, start = ent, vBounds = vbs}
    )

-- blizzard configurations are cyclic, so we can think about generating all the configs once and for all.
allValleys :: Valley -> [[Blizzard]]
allValleys v =
    let vss = valleyBlizzards <$> iterate valleyMove v
     in valleyBlizzards v : takeWhile (/= valleyBlizzards v) (tail vss)

valleyMove :: Valley -> Valley
valleyMove v@(Valley{valleyBlizzards = vbm, valleyBounds = vbs}) =
    v{valleyBlizzards = fmap (blizzardMove vbs) vbm}

blizzardMove :: Position -> Blizzard -> Blizzard
blizzardMove vbounds (NB (x, y)) = NB $ wrap vbounds (x, y - 1)
blizzardMove vbounds (SB (x, y)) = SB $ wrap vbounds (x, y + 1)
blizzardMove vbounds (EB (x, y)) = EB $ wrap vbounds (x - 1, y)
blizzardMove vbounds (WB (x, y)) = WB $ wrap vbounds (x + 1, y)

wrap :: Position -> Position -> Position
wrap (vbx, vby) (x, y)
    | x < 0 = (vbx, y)
    | x > vbx = (0, y)
    | y < 0 = (x, vby)
    | y > vby = (x, 0)
    | otherwise = (x, y)

twentyFourthDecemberSolution1 :: IO Int
twentyFourthDecemberSolution1 = minute . uncurry getResult . searchTillTargetFromValley <$> input

solution2 :: Valley -> Int
solution2 v = minute s1 + minute s2 + minute s3
  where
    (s1, e) = (\(ss, x) -> (getResult ss x, x)) $ searchTillTargetFromValley v
    b = valleys e !! (minute s1 `mod` length (valleys e))
    v' = Valley{valleyBlizzards = b, entrance = target e, valleyBounds = valleyBounds v, exit = start e}
    (s2, e') = (\(ss, x) -> (getResult ss x, x)) $ searchTillTargetFromValley v'
    b' = valleys e' !! (minute s2 `mod` length (valleys e'))
    v'' = Valley{valleyBlizzards = b', entrance = target e', valleyBounds = valleyBounds v, exit = start e'}
    (s3, _) = (\(ss, x) -> (getResult ss x, x)) $ searchTillTargetFromValley v''

twentyFourthDecemberSolution2 :: IO Int
twentyFourthDecemberSolution2 = solution2 <$> input

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
            { entrance = (s_entrance, -1)
            , valleyBlizzards = s_valley
            , exit = (s_exit, length ls - 2)
            , valleyBounds = s_valleyBounds
            }

parseValley :: [String] -> [Blizzard]
parseValley =
    fst
        . foldl
            ( \(m, y) l ->
                ( m ++ parseValleyRow l y
                , y + 1
                )
            )
            ([], 0)

parseValleyRow :: String -> Int -> [Blizzard]
parseValleyRow s y = go s 0
  where
    go [] x = []
    go ('#' : xs) x = go xs x
    go ('.' : xs) x = go xs (x + 1)
    go ('>' : xs) x = WB (x, y) : go xs (x + 1)
    go ('<' : xs) x = EB (x, y) : go xs (x + 1)
    go ('v' : xs) x = SB (x, y) : go xs (x + 1)
    go ('^' : xs) x = NB (x, y) : go xs (x + 1)

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

test = solution2 testInput

testSmall :: Valley
testSmall =
    parseInput
        "#.#####\n\
        \#.....#\n\
        \#>....#\n\
        \#.....#\n\
        \#...v.#\n\
        \#.....#\n\
        \#####.#\n"

blizzardPos :: Blizzard -> Position
blizzardPos (NB p) = p
blizzardPos (SB p) = p
blizzardPos (EB p) = p
blizzardPos (WB p) = p
