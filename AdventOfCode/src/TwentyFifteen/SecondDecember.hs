module TwentyFifteen.SecondDecember where

data Box = Box
  { l :: Int,
    w :: Int,
    h :: Int
  }
  deriving (Show)

input :: IO [Box]
input = fmap parseBox . lines <$> readFile "input/2015/2December.txt"

inputTest :: String
inputTest =
  "2x3x4\n\
  \1x1x10"

parseBox :: String -> Box
parseBox x =
  let (l', rest) = span ('x' /=) x
      (w', rest') = span ('x' /=) (tail rest)
   in Box {l = read l' :: Int, w = read w' :: Int, h = read (tail rest') :: Int}

smallestSide :: Int -> Int -> Int -> Int
smallestSide l' w' h' = min h' $ min l' w'

biggestSide :: Int -> Int -> Int -> Int
biggestSide l' w' h' = max h' $ max l' w'

calculatePaperNeeded :: Box -> Int
calculatePaperNeeded Box {l = l', w = w', h = h'} =
  let (a1, a2, a3) = (l' * w', w' * h', h' * l')
   in 2 * a1 + 2 * a2 + 2 * a3 + smallestSide a1 a2 a3

calculateRibbon :: Box -> Int
calculateRibbon Box {l = l', w = w', h = h'} =
  let extraRibbon = l' * w' * h'
      smallestPerimeter = (2 * l' + 2 * w' + 2 * h') - 2 * biggestSide l' w' h'
   in extraRibbon + smallestPerimeter

test1 = (solution1 . fmap parseBox . lines) inputTest

test2 = (solution2 . fmap parseBox . lines) inputTest

solution1 :: [Box] -> Int
solution1 = sum . fmap calculatePaperNeeded

solution2 :: [Box] -> Int
solution2 = sum . fmap calculateRibbon

secondDecemberSolution1 = solution1 <$> input

secondDecemberSolution2 = solution2 <$> input
