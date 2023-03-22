module TwentyTwentyTwo.TwentyFourthDecember where

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
data State = State {currPos :: Position, minute :: Int}

-- Used when generating the tree of states in search of the goal
data Env = Env {valleys :: [[Blizzard]], target :: Position, visited :: [State], vBounds :: Position}

input :: IO Valley
input = parseInput <$> readFile "input/2022/24December.txt"

-- Generate the next states by generating the next possible positions given the current blizzard
-- and filters by the visited states
nextStates :: State -> Env -> [State]
nextStates (State{currPos = crp, minute = m}) (Env{valleys = vss, target = ext, visited = vis, vBounds = vbs}) = undefined

-- Generate new states based on where expedition can go, or just wait where it is
expeditionMove :: State -> [Blizzard] -> [State]
expeditionMove (State{currPos = crp, minute = m}) bs = undefined

eqState :: Int -> State -> State -> Bool
eqState valleyNum (State{currPos = cr1, minute = m1}) (State{currPos = cr2, minute = m2}) =
    cr1 == cr2 && m1 `mod` valleyNum == m2 `mod` valleyNum

valleyToSearchInit :: Valley -> (State, Env)
valleyToSearchInit v@(Valley{entrance = ent, valleyBounds = vbs, exit = ext}) =
    ( State{currPos = ent, minute = 0}
    , Env{valleys = allValleys v, target = ext, visited = [], vBounds = vbs}
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

test = (!! 5) $ iterate valleyMove testSmall -- (!! 1000) . iterate valleyMove <$> input

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
