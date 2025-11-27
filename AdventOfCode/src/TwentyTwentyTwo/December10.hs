module TwentyTwentyTwo.December10 where

import Data.Bifunctor

data Instruction = Addx Int Int | Noop Int deriving (Show)

data Device = Device
    { regX :: Int
    , instructionBuffer :: [Instruction]
    }
    deriving (Show)

data CRT = CRT
    { index :: Int
    , sprite :: [Int]
    }

input :: IO [Instruction]
input = fmap parseInstruction . lines <$> readFile "input/2022/10December.txt"

parseInstruction :: String -> Instruction
parseInstruction s = case break (== ' ') s of
    ("addx", i) -> Addx (((\x -> read x :: Int) . tail) i) 2
    ("noop", _) -> Noop 1

initialDevice :: [Instruction] -> Device
initialDevice is = Device{regX = 1, instructionBuffer = is}

runDevice :: Device -> [Device]
runDevice = iterate runCycle

runCycle :: Device -> Device
runCycle d@Device{instructionBuffer = []} = d
runCycle d@Device{regX = x, instructionBuffer = (Addx v 0 : is)} = runCycle $ d{regX = x + v, instructionBuffer = is}
runCycle d@Device{regX = x, instructionBuffer = (Addx v t : is)} = d{instructionBuffer = Addx v (t - 1) : is}
runCycle d@Device{instructionBuffer = (Noop 0 : is)} = runCycle $ d{instructionBuffer = is}
runCycle d@Device{instructionBuffer = (Noop t : is)} = d{instructionBuffer = Noop (t - 1) : is}

sampleSignalStrength :: [Device] -> [Int]
sampleSignalStrength ds = (\i -> i * regX (ds !! i)) <$> [20, 60 .. 220]

solution1 :: [Instruction] -> Int
solution1 = sum . sampleSignalStrength . runDevice . initialDevice

december10Solution1 :: IO Int
december10Solution1 = solution1 <$> input

runCRT :: [Device] -> [CRT]
runCRT = fmap (\(d, i) -> buildCRT i (regX d)) . (`zip` [0 ..]) . tail

buildCRT :: Int -> Int -> CRT
buildCRT i r = CRT{index = i `mod` 40, sprite = [r - 1 .. r + 1]}

drawCRT :: CRT -> String
drawCRT c@(CRT{index = i, sprite = xs})
    | i == 0 && i `elem` xs = "\n#"
    | i == 0 && i `notElem` xs = "\n."
    | i /= 0 && i `elem` xs = "#"
    | i /= 0 && i `notElem` xs = "."

solution2 :: [Instruction] -> String
solution2 = concatMap drawCRT . take 240 . runCRT . runDevice . initialDevice

december10Solution2 :: IO String
december10Solution2 = solution2 <$> input

testInputSmall :: [Instruction]
testInputSmall =
    (fmap parseInstruction . lines)
        "noop\n\
        \addx 3\n\
        \addx -5"

testInput :: [Instruction]
testInput =
    (fmap parseInstruction . lines)
        "addx 15\n\
        \addx -11\n\
        \addx 6\n\
        \addx -3\n\
        \addx 5\n\
        \addx -1\n\
        \addx -8\n\
        \addx 13\n\
        \addx 4\n\
        \noop\n\
        \addx -1\n\
        \addx 5\n\
        \addx -1\n\
        \addx 5\n\
        \addx -1\n\
        \addx 5\n\
        \addx -1\n\
        \addx 5\n\
        \addx -1\n\
        \addx -35\n\
        \addx 1\n\
        \addx 24\n\
        \addx -19\n\
        \addx 1\n\
        \addx 16\n\
        \addx -11\n\
        \noop\n\
        \noop\n\
        \addx 21\n\
        \addx -15\n\
        \noop\n\
        \noop\n\
        \addx -3\n\
        \addx 9\n\
        \addx 1\n\
        \addx -3\n\
        \addx 8\n\
        \addx 1\n\
        \addx 5\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \addx -36\n\
        \noop\n\
        \addx 1\n\
        \addx 7\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \addx 2\n\
        \addx 6\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \addx 1\n\
        \noop\n\
        \noop\n\
        \addx 7\n\
        \addx 1\n\
        \noop\n\
        \addx -13\n\
        \addx 13\n\
        \addx 7\n\
        \noop\n\
        \addx 1\n\
        \addx -33\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \addx 2\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \addx 8\n\
        \noop\n\
        \addx -1\n\
        \addx 2\n\
        \addx 1\n\
        \noop\n\
        \addx 17\n\
        \addx -9\n\
        \addx 1\n\
        \addx 1\n\
        \addx -3\n\
        \addx 11\n\
        \noop\n\
        \noop\n\
        \addx 1\n\
        \noop\n\
        \addx 1\n\
        \noop\n\
        \noop\n\
        \addx -13\n\
        \addx -19\n\
        \addx 1\n\
        \addx 3\n\
        \addx 26\n\
        \addx -30\n\
        \addx 12\n\
        \addx -1\n\
        \addx 3\n\
        \addx 1\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \addx -9\n\
        \addx 18\n\
        \addx 1\n\
        \addx 2\n\
        \noop\n\
        \noop\n\
        \addx 9\n\
        \noop\n\
        \noop\n\
        \noop\n\
        \addx -1\n\
        \addx 2\n\
        \addx -37\n\
        \addx 1\n\
        \addx 3\n\
        \noop\n\
        \addx 15\n\
        \addx -21\n\
        \addx 22\n\
        \addx -6\n\
        \addx 1\n\
        \noop\n\
        \addx 2\n\
        \addx 1\n\
        \noop\n\
        \addx -10\n\
        \noop\n\
        \noop\n\
        \addx 20\n\
        \addx 1\n\
        \addx 2\n\
        \addx 2\n\
        \addx -6\n\
        \addx -11\n\
        \noop\n\
        \noop\n\
        \noop"
