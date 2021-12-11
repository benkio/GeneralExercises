module TwentyTwentyOne.TenthDecember where

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M (elems, fromList, lookup)
import Data.Maybe (fromJust)
import Debug.Trace

input :: IO String
input = readFile "input/2021/10December.txt"

inputTest :: String
inputTest =
  "[({(<(())[]>[[{[]{<()<>>\n\
  \[(()[<>])]({[<{<<[]>>(\n\
  \{([(<{}[<>[]}>{[]{[(<()>\n\
  \(((({<>}<{<{<>}{[]{[]{}\n\
  \[[<[([]))<([[{}[[()]]]\n\
  \[{[{({}]{}}([{[{{{}}([]\n\
  \{<[[]]>}<{[{[{[]{()[[[]\n\
  \[<(<(<(<{}))><([]([]()\n\
  \<{([([[(<>()){}]>(<<{{\n\
  \<{([{{}}[<[[[<>{}]]]>[]]"

data Line = Line [Chunk] | ErrorLine ErrorType deriving (Show)

data ErrorType = Miss Char | Corrupted Char deriving (Show)

data Chunk = Chunk
  { open :: Char,
    content :: [Chunk],
    close :: Char
  }
  deriving (Show)

emptyChunk :: Chunk
emptyChunk = Chunk {open = ' ', content = [], close = ' '}

parseInput :: String -> [Line]
parseInput = fmap (parseLine (Line [])) . lines

lineToString :: Line -> String
lineToString (Line cs) = foldl (\v c -> v ++ chunkToString c) "" cs
lineToString l = show l

chunkToString :: Chunk -> String
chunkToString (Chunk {open = o, content = cs, close = c}) = o : (foldl (\v c -> v ++ chunkToString c) "" cs) ++ [c]

parseLine :: Line -> String -> Line
parseLine l [] = l
parseLine (Line cs) xs =
  let ecr = parseChunk xs
   in case ecr of
        Left c -> ErrorLine c
        Right (c, []) -> Line (cs ++ [c])
        Right (c, r) -> parseLine (Line (cs ++ [c])) r

pairMap :: Map Char Char
pairMap = M.fromList [('(', ')'), ('{', '}'), ('[', ']'), ('<', '>')]

parseChunk :: String -> Either ErrorType (Chunk, String)
parseChunk [] = Right (emptyChunk, "")
parseChunk ('(' : xs) = parseChunkNested xs '('
parseChunk ('<' : xs) = parseChunkNested xs '<'
parseChunk ('{' : xs) = parseChunkNested xs '{'
parseChunk ('[' : xs) = parseChunkNested xs '['
parseChunk (x : _) = Left (Corrupted x)

parseChunkNested :: String -> Char -> Either ErrorType (Chunk, String)
parseChunkNested [] c = Left (Miss (fromJust (M.lookup c pairMap)))
parseChunkNested (')' : xs) '(' = Right (Chunk {open = '(', content = [], close = ')'}, xs)
parseChunkNested ('>' : xs) '<' = Right (Chunk {open = '<', content = [], close = '>'}, xs)
parseChunkNested ('}' : xs) '{' = Right (Chunk {open = '{', content = [], close = '}'}, xs)
parseChunkNested (']' : xs) '[' = Right (Chunk {open = '[', content = [], close = ']'}, xs)
parseChunkNested (x : xs) c
  | x `elem` M.elems pairMap = Left (Corrupted x)
  | otherwise = case parseContentChunk (x : xs) c of
    Left c' -> Left c'
    Right (cs, x' : xs') -> Right (Chunk {open = c, content = cs, close = x'}, xs')

parseContentChunk :: String -> Char -> Either ErrorType ([Chunk], String)
parseContentChunk [] c = Left (Miss (fromJust (M.lookup c pairMap)))
parseContentChunk (x : xs) c
  | x == fromJust (M.lookup c pairMap) = Right ([], x : xs)
  | otherwise = do
    (ch, xs') <- parseChunk (x : xs)
    (cs, xs'') <- parseContentChunk xs' c
    return (ch : cs, xs'')

corruptedScore :: Line -> Int
corruptedScore (ErrorLine (Corrupted ')')) = 3
corruptedScore (ErrorLine (Corrupted ']')) = 57
corruptedScore (ErrorLine (Corrupted '}')) = 1197
corruptedScore (ErrorLine (Corrupted '>')) = 25137
corruptedScore _ = 0

isCorruptedLine :: Line -> Bool
isCorruptedLine (ErrorLine (Miss _)) = False
isCorruptedLine (Line _) = False
isCorruptedLine _ = True

isIncompleteLine :: Line -> Bool
isIncompleteLine (ErrorLine (Corrupted _)) = False
isIncompleteLine (Line _) = False
isIncompleteLine _ = True

isGoodLine :: Line -> Bool
isGoodLine (Line _) = True
isGoodLine _ = False

solution1 :: String -> Int
solution1 = sum . fmap corruptedScore . filter isCorruptedLine . parseInput

tenthDecemberSolution1 :: IO Int
tenthDecemberSolution1 = solution1 <$> input

fixLine :: String -> String
fixLine s = case parseLine (Line []) s of
  ErrorLine (Miss c) -> c : fixLine (s ++ [c])
  Line _ -> []
  ErrorLine (Corrupted _) -> error $ "Can't fix corrupted lines: " ++ s

fixScore :: String -> Int
fixScore = foldl (\v c -> v * 5 + fixScore' c) 0

fixScore' :: Char -> Int
fixScore' ')' = 1
fixScore' ']' = 2
fixScore' '}' = 3
fixScore' '>' = 4
fixScore' _ = 0

solution2 :: String -> Int
solution2 i =
  let input_lines = zip (lines i) (parseInput i)
      fixedLinesScored = (sort . fmap (\(i, _) -> (fixScore . fixLine) i) . filter (isIncompleteLine . snd)) input_lines
   in fixedLinesScored !! (length fixedLinesScored `div` 2)

tenthDecemberSolution2 :: IO Int
tenthDecemberSolution2 = solution2 <$> input
