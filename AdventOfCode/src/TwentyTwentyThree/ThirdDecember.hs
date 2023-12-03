module TwentyTwentyThree.ThirdDecember where

import Data.List
import Debug.Trace

data EngineSchema = PN {pnValue :: Int, coords :: [(Int, Int)]} | S {sValue :: Char, coord :: (Int, Int)} deriving (Show, Eq)

engineSchemaCoords :: EngineSchema -> [(Int, Int)]
engineSchemaCoords (PN{coords = cs}) = cs
engineSchemaCoords (S{coord = c}) = [c]

engineSchemaNumValue :: EngineSchema -> Int
engineSchemaNumValue (PN{pnValue = v}) = v

engineSchemaSymbolValue :: EngineSchema -> Char
engineSchemaSymbolValue (S{sValue = v}) = v

engineSchemaIsNumber :: EngineSchema -> Bool
engineSchemaIsNumber (S{coord = c}) = False
engineSchemaIsNumber (PN{coords = cs}) = True

input :: IO [EngineSchema]
input = parseInput <$> readFile "input/2023/3December.txt"

-- 2289
parseInput :: String -> [EngineSchema]
parseInput =
    groupEngineSchemas
        . parseString
  where
    parseString :: String -> [((Int, Int), Char)]
    parseString = concatMap (\(y, l) -> (fmap (\(x, c) -> ((x, y), c)) . zip [0 ..]) l) . zip [0 ..] . lines
    groupEngineSchemas [] = []
    groupEngineSchemas (((x, y), c) : cs)
        | c == '.' = groupEngineSchemas cs
        | c `notElem` ['0' .. '9'] = S{sValue = c, coord = (x, y)} : groupEngineSchemas cs
        | otherwise =
            let (num, rest) = span ((`elem` ['0' .. '9']) . snd) cs
             in PN{pnValue = read (c : fmap snd num) :: Int, coords = (x, y) : fmap fst num} : groupEngineSchemas rest
testInput :: [EngineSchema]
testInput =
    parseInput
        "467..114..\n\
        \...*......\n\
        \..35..633.\n\
        \......#...\n\
        \617*......\n\
        \.....+.58.\n\
        \..592.....\n\
        \......755.\n\
        \...$.*....\n\
        \.664.598.."

adjacent :: (Int, Int) -> (Int, Int) -> Bool
adjacent c (x, y) = c `elem` [(a, b) | a <- [(x - 1) .. (x + 1)], b <- [(y - 1) .. (y + 1)]]

adjacentEngineSchema :: EngineSchema -> EngineSchema -> Bool
adjacentEngineSchema es es' = any (\c -> any (c `adjacent`) (engineSchemaCoords es')) (engineSchemaCoords es)

filterEngineSchemas :: [EngineSchema] -> [EngineSchema]
filterEngineSchemas es = filter (\e -> any (e `adjacentEngineSchema`) symbls) nums
  where
    (nums, symbls) = partition engineSchemaIsNumber es

solution1 :: [EngineSchema] -> Int
solution1 = sum . fmap engineSchemaNumValue . filterEngineSchemas

test :: [EngineSchema] -> [EngineSchema]
test es = ((filter engineSchemaIsNumber es \\) . filterEngineSchemas) es

-- 443544 too low
-- 444707 too low
thirdDecemberSolution1 :: IO Int
thirdDecemberSolution1 = solution1 <$> input

data Gear = G {gv1 :: Int, gv2 :: Int} deriving (Show)

-- filterEngineSchemaGears :: [EngineSchema] -> [Gear]
filterEngineSchemaGears es = (fmap toGear . filter ((== 2) . length) . fmap findGearPart) gearCenter
  where
    (nums, symbls) = partition engineSchemaIsNumber es
    (gearCenter, _) = partition ((== '*') . engineSchemaSymbolValue) symbls
    findGearPart g = filter (`adjacentEngineSchema` g) nums
    toGear [pn, pn'] = G{gv1 = engineSchemaNumValue pn, gv2 = engineSchemaNumValue pn'}

solution2 = sum . fmap gearRatio . filterEngineSchemaGears
  where
    gearRatio (G{gv1 = x, gv2 = y}) = x * y

thirdDecemberSolution2 :: IO Int
thirdDecemberSolution2 = solution2 <$> input
