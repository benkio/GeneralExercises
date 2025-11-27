-------------------------------------------------------------------------------
--                           Advent Of Code - day 12                          --
-------------------------------------------------------------------------------
module TwentyTwenty.December12 where

type Instruction = Either Move Direction

data Move
    = N Int
    | S Int
    | E Int
    | W Int
    | F Int
    deriving (Show)

data Direction
    = L Int
    | R Int
    deriving (Show)

-- I should have used a Reader, but I don't want to add packages
type State = (Move, (Move, Move))

type ViewPointState = ((Move, Move), (Move, Move))

initialState :: State
initialState = (E 0, (E 0, N 0))

viewPointInitialState :: ViewPointState
viewPointInitialState = ((E 10, N 1), (E 0, N 0))

interpreter :: Either Move Direction -> State -> State
interpreter (Right d') (d, p) = (directionInterpreter d' d, p)
interpreter (Left (F v)) (d, p) = (d, moveInterpreter (setMoveValue d v) p)
interpreter (Left m) (d, p) = (d, moveInterpreter m p)

viewPointInterpreter ::
    Either Move Direction -> ViewPointState -> ViewPointState
viewPointInterpreter (Right d') ((vp1, vp2), p) =
    (fixRotation (directionInterpreter d' vp1, directionInterpreter d' vp2), p)
viewPointInterpreter (Left (F v)) ((vp1, vp2), (p1, p2)) =
    ( (vp1, vp2)
    , ( moveInterpreter (multiplyMoveValue vp2 v)
            . moveInterpreter (multiplyMoveValue vp1 v)
      )
        (p1, p2)
    )
viewPointInterpreter (Left m) (vp, p) = (moveInterpreter m vp, p)

-- interpret Direction --------------
directionInterpreter :: Direction -> Move -> Move
directionInterpreter dir@(L d) p@(E v)
    | d `div` 90 == 1 = N v
    | d `div` 90 == 2 = W v
    | d `div` 90 == 3 = S v
    | otherwise =
        error $
            "unexpected direction " ++ show dir ++ " applied to position " ++ show p
directionInterpreter dir@(L d) p@(W v)
    | d `div` 90 == 1 = S v
    | d `div` 90 == 2 = E v
    | d `div` 90 == 3 = N v
    | otherwise =
        error $
            "unexpected direction " ++ show dir ++ " applied to position " ++ show p
directionInterpreter dir@(L d) p@(S v)
    | d `div` 90 == 1 = E v
    | d `div` 90 == 2 = N v
    | d `div` 90 == 3 = W v
    | otherwise =
        error $
            "unexpected direction " ++ show dir ++ " applied to position " ++ show p
directionInterpreter dir@(L d) p@(N v)
    | d `div` 90 == 1 = W v
    | d `div` 90 == 2 = S v
    | d `div` 90 == 3 = E v
    | otherwise =
        error $
            "unexpected direction " ++ show dir ++ " applied to position " ++ show p
directionInterpreter dir@(R d) p@(E v)
    | d `div` 90 == 1 = S v
    | d `div` 90 == 2 = W v
    | d `div` 90 == 3 = N v
    | otherwise =
        error $
            "unexpected direction " ++ show dir ++ " applied to position " ++ show p
directionInterpreter dir@(R d) p@(W v)
    | d `div` 90 == 1 = N v
    | d `div` 90 == 2 = E v
    | d `div` 90 == 3 = S v
    | otherwise =
        error $
            "unexpected direction " ++ show dir ++ " applied to position " ++ show p
directionInterpreter dir@(R d) p@(S v)
    | d `div` 90 == 1 = W v
    | d `div` 90 == 2 = N v
    | d `div` 90 == 3 = E v
    | otherwise =
        error $
            "unexpected direction " ++ show dir ++ " applied to position " ++ show p
directionInterpreter dir@(R d) p@(N v)
    | d `div` 90 == 1 = E v
    | d `div` 90 == 2 = S v
    | d `div` 90 == 3 = W v
    | otherwise =
        error $
            "unexpected direction " ++ show dir ++ " applied to position " ++ show p
directionInterpreter dir p =
    error $
        "unexpected direction " ++ show dir ++ " applied to position " ++ show p

-- interpret move -------------------
moveInterpreter :: Move -> (Move, Move) -> (Move, Move)
moveInterpreter (E v) (eo, ns) = (interpretMove (E v) eo, ns)
moveInterpreter (W v) (eo, ns) = (interpretMove (W v) eo, ns)
moveInterpreter (S v) (eo, ns) = (eo, interpretMove (S v) ns)
moveInterpreter (N v) (eo, ns) = (eo, interpretMove (N v) ns)
moveInterpreter m s =
    error $ "unexpected " ++ show m ++ " applied to position " ++ show s

interpretMove :: Move -> Move -> Move
interpretMove (S v) (N v') =
    if (v' - v) < 0
        then S (abs (v' - v))
        else N (v' - v)
interpretMove (S v) (S v') = S (v + v')
interpretMove (N v) (S v') =
    if (v' - v) < 0
        then N (abs (v' - v))
        else S (v' - v)
interpretMove (N v) (N v') = N (v + v')
interpretMove (E v) (W v') =
    if (v' - v) < 0
        then E (abs (v' - v))
        else W (v' - v)
interpretMove (E v) (E v') = E (v + v')
interpretMove (W v) (E v') =
    if (v' - v) < 0
        then W (abs (v' - v))
        else E (v' - v)
interpretMove (W v) (W v') = W (v + v')
interpretMove m m' =
    error $ "unexpected move applied " ++ show m ++ " to position " ++ show m'

createInstruction :: String -> Instruction
createInstruction ('N' : v) = Left $ N (read v :: Int)
createInstruction ('S' : v) = Left $ S (read v :: Int)
createInstruction ('E' : v) = Left $ E (read v :: Int)
createInstruction ('W' : v) = Left $ W (read v :: Int)
createInstruction ('F' : v) = Left $ F (read v :: Int)
createInstruction ('L' : v) = Right $ L (read v :: Int)
createInstruction ('R' : v) = Right $ R (read v :: Int)
createInstruction s = error $ "not recognized input " ++ s

setMoveValue :: Move -> Int -> Move
setMoveValue (N _) v = N v
setMoveValue (S _) v = S v
setMoveValue (E _) v = E v
setMoveValue (W _) v = W v
setMoveValue m v = error $ "cannot set a value " ++ show v ++ " for " ++ show m

multiplyMoveValue :: Move -> Int -> Move
multiplyMoveValue (N x) v = N (v * x)
multiplyMoveValue (S x) v = S (v * x)
multiplyMoveValue (E x) v = E (v * x)
multiplyMoveValue (W x) v = W (v * x)
multiplyMoveValue m v =
    error $ "cannot multiply a value " ++ show v ++ " for " ++ show m

getMoveValue :: Move -> Int
getMoveValue (N v) = v
getMoveValue (S v) = v
getMoveValue (E v) = v
getMoveValue (W v) = v
getMoveValue m = error $ "cannot get a value for " ++ show m

input :: IO [Instruction]
input = fmap createInstruction . lines <$> readFile "input/2020/12December.txt"

december12Solution1 :: IO Int
december12Solution1 =
    (\(eo, ns) -> getMoveValue eo + getMoveValue ns)
        . snd
        . foldl (flip interpreter) initialState
        <$> input

december12Solution2 :: IO Int
december12Solution2 =
    (\(eo, ns) -> getMoveValue eo + getMoveValue ns)
        . snd
        . foldl (flip viewPointInterpreter) viewPointInitialState
        <$> input

isNorthSouth :: Move -> Bool
isNorthSouth (N _) = True
isNorthSouth (S _) = True
isNorthSouth _ = False

fixRotation :: (Move, Move) -> (Move, Move)
fixRotation (eo, ns)
    | isNorthSouth eo && (not . isNorthSouth) ns = (ns, eo)
    | otherwise = (eo, ns)

testInput =
    fmap createInstruction . lines $
        "F10\n\
        \N3\n\
        \F7\n\
        \R90\n\
        \F11"
