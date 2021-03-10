{-# LANGUAGE TupleSections #-}

module TwentySixteen.EighteenthDecember where

input :: IO String
input = init <$> readFile "input/2016/18December.txt"

calculateTile :: Int -> [(Char, Int)] -> Char
calculateTile index prevRow
  | prevRowFiltered == "^^." = '^'
  | prevRowFiltered == ".^^" = '^'
  | prevRowFiltered == "^.." = '^'
  | prevRowFiltered == "..^" = '^'
  | otherwise = '.'
  where
    prevRowFiltered = concatMap ((: []) . fst) (prevRowNearIndex index prevRow)

prevRowNearIndex :: Int -> [(Char, Int)] -> [(Char, Int)]
prevRowNearIndex i =
  amendPrevRowNearIndex i
    . filter (\(_, x) -> x `elem` [(i - 1) .. (i + 1)])

amendPrevRowNearIndex :: Int -> [(Char, Int)] -> [(Char, Int)]
amendPrevRowNearIndex index prevRowNearIndex
  | (not . any (\(_, x) -> x == index - 1)) prevRowNearIndex = ('.', index - 1) : prevRowNearIndex
  | (not . any (\(_, x) -> x == index + 1)) prevRowNearIndex = prevRowNearIndex ++ [('.', index - 1)]
  | otherwise = prevRowNearIndex

generateNextRowAccuml :: (Int, String) -> (Int, String)
generateNextRowAccuml (acc, lastRow) =
  let lastRowWithIndex = lastRow `zip` [0 ..]
      indexes = fmap snd lastRowWithIndex
   in (acc + (length . filter ('.' ==)) lastRow, fmap (`calculateTile` lastRowWithIndex) indexes)

solution :: Int -> String -> Int
solution totalRows =
  fst
    . (!! totalRows)
    . iterate generateNextRowAccuml
    . (0,)

solution1Test :: Bool
solution1Test = solution 10 ".^^.^.^^^^" == 38

eighteenthDecemberSolution1 :: IO Int
eighteenthDecemberSolution1 = solution 40 <$> input

eighteenthDecemberSolution2 :: IO Int
eighteenthDecemberSolution2 = solution 400000 <$> input
