module TwentySixteen.FirstDecember where

data Instruction
    = R Int
    | L Int
    deriving (Show)

data Face
    = N
    | S
    | E
    | W
    deriving (Show)

input :: IO [Instruction]
input = parseInput <$> readFile "input/2016/1December.txt"

parseInput :: String -> [Instruction]
parseInput = fmap (parseInstruction . takeWhile (',' /=)) . words
  where
    parseInstruction :: String -> Instruction
    parseInstruction ('L' : x) = L (read x :: Int)
    parseInstruction ('R' : x) = R (read x :: Int)
    parseInstruction x = error $ "Instruction not recognized: " ++ x

move :: [Instruction] -> (Int, Int)
move = fst . foldl step ((0, 0), N)

step :: ((Int, Int), Face) -> Instruction -> ((Int, Int), Face)
step ((x, y), N) (L z) = ((x - z, y), W)
step ((x, y), S) (L z) = ((x + z, y), E)
step ((x, y), W) (L z) = ((x, y - z), S)
step ((x, y), E) (L z) = ((x, y + z), N)
step ((x, y), N) (R z) = ((x + z, y), E)
step ((x, y), S) (R z) = ((x - z, y), W)
step ((x, y), W) (R z) = ((x, y + z), N)
step ((x, y), E) (R z) = ((x, y - z), S)

solution1 :: [Instruction] -> Int
solution1 = uncurry (+) . move

firstDecemberSolution1 :: IO Int
firstDecemberSolution1 = solution1 <$> input

moveNTrack :: [Instruction] -> [(Int, Int)]
moveNTrack = fmap fst . scanl step ((0, 0), N)

generateMiddlePoints :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
generateMiddlePoints (x, y) (a, b) = do
    c <-
        if x <= a
            then [x .. a]
            else [x, x - 1 .. a]
    d <-
        if y <= b
            then [y .. b]
            else [y, y - 1 .. b]
    return (c, d)

solution2 :: [Instruction] -> Int
solution2 =
    uncurry (+)
        . findDuplicate
        . foldl1 (\l l' -> init l ++ generateMiddlePoints (last l) (head l'))
        . fmap (: [])
        . moveNTrack

findDuplicate :: [(Int, Int)] -> (Int, Int)
findDuplicate [] = (0, 0)
findDuplicate (x : xs) =
    if x `elem` xs
        then x
        else findDuplicate xs

firstDecemberSolution2 :: IO Int
firstDecemberSolution2 = solution2 <$> input
