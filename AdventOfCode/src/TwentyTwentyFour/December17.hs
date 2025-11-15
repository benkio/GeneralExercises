module TwentyTwentyFour.December17 where

-- import Debug.Trace (trace, traceShow, traceShowId)

import Data.Bifunctor (second)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust, isNothing, maybeToList)
import Lib.Bit (
    bitWiseXorWithBase,
    fillBitToN,
    fromBaseBit,
    toBaseBit,
 )

data Computer = C
    { regA :: Int
    , regB :: Int
    , regC :: Int
    , program :: [Int]
    , instructionPointer :: Int
    }
    deriving (Show, Eq, Ord)

data Operand
    = Literal Int
    | Combo Int
    deriving (Show)

data Opcode
    = Adv Operand
    | Blx Operand
    | Bst Operand
    | Jnz Operand
    | Bxc
    | Out Operand
    | Bdv Operand
    | Cdv Operand
    deriving (Show)

type Instruction = Opcode

input :: IO Computer
input = parseInput <$> readFile "input/2024/December17.txt"

emptyComputer :: Computer
emptyComputer =
    C
        { regA = 0
        , regB = 0
        , regC = 0
        , program = []
        , instructionPointer = 0
        }

toOpcode :: Int -> Int -> Opcode
toOpcode 0 opcode = Adv $ Combo opcode
toOpcode 1 opcode = Blx $ Literal opcode
toOpcode 2 opcode = Bst $ Combo opcode
toOpcode 3 opcode = Jnz $ Literal opcode
toOpcode 4 _ = Bxc
toOpcode 5 opcode = Out $ Combo opcode
toOpcode 6 opcode = Bdv $ Combo opcode
toOpcode 7 opcode = Cdv $ Combo opcode

opcodeGetOperand :: Opcode -> Maybe Operand
opcodeGetOperand (Adv op) = Just op
opcodeGetOperand (Blx op) = Just op
opcodeGetOperand (Bst op) = Just op
opcodeGetOperand (Jnz op) = Just op
opcodeGetOperand Bxc = Nothing
opcodeGetOperand (Out op) = Just op
opcodeGetOperand (Bdv op) = Just op
opcodeGetOperand (Cdv op) = Just op

runOperand :: Operand -> Computer -> Int
runOperand (Literal x) _ = x
runOperand (Combo x) computer
    | x `elem` [0 .. 3] = x
    | x == 4 = readRegister computer 'A'
    | x == 5 = readRegister computer 'B'
    | x == 6 = readRegister computer 'C'
    | x == 7 = error "got reserved combo operand 7"

readRegister :: Computer -> Char -> Int
readRegister computer 'A' = regA computer
readRegister computer 'B' = regB computer
readRegister computer 'C' = regC computer

writeRegister :: Computer -> Char -> Int -> Computer
writeRegister computer 'A' val = computer{regA = val}
writeRegister computer 'B' val = computer{regB = val}
writeRegister computer 'C' val = computer{regC = val}

buildInstruction :: Int -> Int -> Instruction
buildInstruction = toOpcode

nextInstruction :: Computer -> Maybe Instruction
nextInstruction C{program = p, instructionPointer = ip} =
    if isHaltProgram p ip then Nothing else Just $ buildInstruction (p !! ip) (p !! (ip + 1))

isHaltProgram :: [Int] -> Int -> Bool
isHaltProgram program instructionPointer
    | length program <= instructionPointer = True
    | (length program - 1) == instructionPointer = error "Strange instruction pointer position"
    | otherwise = False

moveInstructionPointer :: Computer -> Computer
moveInstructionPointer c@(C{instructionPointer = ip}) = c{instructionPointer = ip + 2}

runInstruction :: Instruction -> Computer -> (Computer, Maybe Int)
runInstruction (Adv op) computer =
    ( moveInstructionPointer $ writeRegister computer 'A' $ readRegister computer 'A' `div` 2 ^ runOperand op computer
    , Nothing
    )
runInstruction (Blx op) computer =
    ( moveInstructionPointer $ writeRegister computer 'B' $ bitWiseXorWithBase 8 (readRegister computer 'B') (runOperand op computer)
    , Nothing
    )
runInstruction (Bst op) computer =
    ( moveInstructionPointer $ writeRegister computer 'B' $ runOperand op computer `mod` 8
    , Nothing
    )
runInstruction (Jnz op) computer
    | readRegister computer 'A' == 0 = (moveInstructionPointer computer, Nothing)
    | otherwise = (computer{instructionPointer = runOperand op computer}, Nothing)
runInstruction Bxc computer =
    ( moveInstructionPointer $ writeRegister computer 'B' $ bitWiseXorWithBase 8 (readRegister computer 'B') (readRegister computer 'C')
    , Nothing
    )
runInstruction (Out op) computer =
    ( moveInstructionPointer computer
    , Just $ runOperand op computer `mod` 8
    )
runInstruction (Bdv op) computer =
    ( moveInstructionPointer $ writeRegister computer 'B' $ readRegister computer 'A' `div` 2 ^ runOperand op computer
    , Nothing
    )
runInstruction (Cdv op) computer =
    ( moveInstructionPointer $ writeRegister computer 'C' $ readRegister computer 'A' `div` 2 ^ runOperand op computer
    , Nothing
    )

runProgram :: Computer -> (Computer, [Int])
runProgram computer =
    until
        (isNothing . nextInstruction . fst)
        (\(c, output) -> (second ((output ++) . maybeToList) . (`runInstruction` c) . fromJust . nextInstruction) c)
        (computer, [])

intOutputToString :: [Int] -> String
intOutputToString = tail . init . show

solution1 :: Computer -> String
solution1 = intOutputToString . snd . runProgram

december17Solution1 :: IO String
december17Solution1 = solution1 <$> input

bruteForceSolution2 :: Int -> Computer -> Int
bruteForceSolution2 n c =
    readRegister resultComputer 'A'
  where
    resultComputer =
        until
            ( \y ->
                (intOutputToString . program) c == solution1 y
            )
            ( \x ->
                -- trace ("regA: " ++ show (regA x) ++ " - " ++ show (solution1 x)) $
                writeRegister x 'A' (regA x + 1)
            )
            (writeRegister c 'A' n)

runAt :: Int -> Computer -> String
runAt regAVal c = solution1 $ c{regA = regAVal}

search :: String -> Computer -> Int
search t c =
    regA . fst $
        until
            (\(y, _) -> condition y)
            ( \(x, exp) ->
                let output = solution1 x
                    distanceToTarget = length t - length output
                 in ( -- trace ("regA: " ++ show (regA x) ++ " - " ++ show output ++ " - " ++ show exp) $
                      writeRegister x 'A' exp
                    , next exp distanceToTarget output
                    )
            )
            (writeRegister c 'A' 0, 0)
  where
    condition c =
        let out = solution1 c in (out == t) || fst (calculateSearchChange (equalValues t out) numT (stringToInt out)) < 100
    numT = stringToInt t
    equalValues x y = length . takeWhile id $ zipWith (==) ((reverse . filter (/= ',')) x) ((reverse . filter (/= ',')) y)
    next x y out
        | y >= 2 = (x + 1) * y
        | y == 1 = x + (x `div` 2)
        | y == 0 -- trace ("change " ++ show increase) $
            =
            x + fst increase
        | y < 0 = x - (x `div` 2)
        | otherwise = error ("wtf " ++ show y)
      where
        numOut = stringToInt out
        increase = calculateSearchChange (equalValues t out) numT numOut

calculateSearchChange :: Int -> Int -> Int -> (Int, Int)
calculateSearchChange equalValues xs ys =
    if differenceMod == 0 then (xs - ys, limitCoeff) else (differenceMod, limitCoeff)
  where
    limitCoeff = max 0 (length (show xs) - equalValues - 1)
    differenceMod = (xs - ys) `mod` (8 ^ limitCoeff)

stringToInt :: String -> Int
stringToInt = read . reverse . filter (/= ',')

solution2 :: Computer -> Int
solution2 c =
    regA $
        until
            ( \y ->
                target == solution1 y -- trace ("sol: " ++ show (solution1 y) ++ " RegA: " ++ show (regA y))
            )
            (\x -> writeRegister x 'A' (regA x + 1))
            (writeRegister c 'A' (goodApproximation + movingBack))
  where
    goodApproximation = search target c
    movingBack = -1000
    target = intOutputToString (program c)

{-
regA: 190383954621537 - "0,3,6,5,4,3,5,7,3,5,3,7,5,5,3,0" - 190390908265704
change 3449030288
regA: 190390908265704 - "7,1,7,5,5,1,3,7,6,5,7,3,5,5,3,0" - 190394357295992
change 2373967337
regA: 190394357295992 - "3,1,5,0,5,6,5,5,7,5,1,3,5,5,3,0" - 190396731263329
-}
december17Solution2 :: IO Int
december17Solution2 = solution2 <$> input

parseInput :: String -> Computer
parseInput = parseComputer . splitOn "\n"
  where
    dropTillColon = tail . dropWhile (/= ':')
    dropTillComma = tail . dropWhile (/= ',')
    parseRegister = (\x -> read x :: Int) . dropTillColon
    parseProgram = fmap (\x -> read x :: Int) . splitOn "," . dropTillColon
    parseComputer :: [String] -> Computer
    parseComputer xs =
        C
            { regA = parseRegister (head xs)
            , regB = parseRegister (xs !! 1)
            , regC = parseRegister (xs !! 2)
            , program = parseProgram (xs !! 4)
            , instructionPointer = 0
            }

testInput :: Computer
testInput =
    parseInput
        "Register A: 729\n\
        \Register B: 0\n\
        \Register C: 0\n\
        \\n\
        \Program: 0,1,5,4,3,0\n"

testInput' :: Computer
testInput' =
    parseInput
        "Register A: 2024\n\
        \Register B: 0\n\
        \Register C: 0\n\
        \\n\
        \Program: 0,3,5,4,3,0\n"

testComputers :: [Bool]
testComputers =
    fmap (\(c, check) -> check (runProgram c)) computers
  where
    computers =
        [ (emptyComputer{program = [2, 6], regC = 9}, \(c, _) -> regB c == 1)
        , (emptyComputer{program = [5, 0, 5, 1, 5, 4], regA = 10}, \(_, out) -> out == [0 .. 2])
        , (emptyComputer{program = [0, 1, 5, 4, 3, 0], regA = 2024}, \(c, out) -> out == [4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0] && regA c == 0)
        , (emptyComputer{program = [1, 7], regB = 29}, \(c, _) -> regB c == 26)
        , (emptyComputer{program = [4, 0], regB = 2024, regC = 43690}, \(c, _) -> regB c == 44354)
        ]
