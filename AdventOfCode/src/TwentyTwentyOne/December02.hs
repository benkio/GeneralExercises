module TwentyTwentyOne.December02 where

data Command = Up Int | Down Int | Forward Int deriving (Show)

data Position = Position
    { horizontal :: Int
    , depth :: Int
    , aim :: Int
    }

initialPosition :: Position
initialPosition = Position{horizontal = 0, depth = 0, aim = 0}

parseCommand :: String -> Command
parseCommand s
    | keyword == "forward" = Forward (read value :: Int)
    | keyword == "down" = Down (read value :: Int)
    | keyword == "up" = Up (read value :: Int)
    | otherwise = error "Command not recognized"
  where
    [keyword, value] = words s

input :: IO [Command]
input = fmap parseCommand . lines <$> readFile "input/2021/2December.txt"

testInput :: [Command]
testInput =
    parseCommand
        <$> lines
            "forward 5\n\
            \down 5\n\
            \forward 8\n\
            \up 3\n\
            \down 8\n\
            \forward 2"

executeCommand :: Position -> Command -> Position
executeCommand p (Forward x) = p{horizontal = horizontal p + x}
executeCommand p (Up x) = p{depth = depth p - x}
executeCommand p (Down x) = p{depth = depth p + x}

executeCommands :: Position -> [Command] -> Position
executeCommands = foldl executeCommand

solution :: (Position -> [Command] -> Position) -> [Command] -> Int
solution ec = (\p -> horizontal p * depth p) . ec initialPosition

december02Solution1 :: IO Int
december02Solution1 = solution executeCommands <$> input

executeCommand' :: Position -> Command -> Position
executeCommand' p (Forward x) = p{horizontal = horizontal p + x, depth = depth p + aim p * x}
executeCommand' p (Up x) = p{aim = aim p - x}
executeCommand' p (Down x) = p{aim = aim p + x}

executeCommands' :: Position -> [Command] -> Position
executeCommands' = foldl executeCommand'

december02Solution2 :: IO Int
december02Solution2 = solution executeCommands' <$> input
