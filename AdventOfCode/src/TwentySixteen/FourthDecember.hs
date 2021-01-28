module TwentySixteen.FourthDecember where

import Data.List
import Data.Maybe

data Room =
  Room
    { rNames :: [String]
    , rId :: Int
    , rChecksum :: String
    }
  deriving (Show)

input :: IO [Room]
input = fmap parseRoom . lines <$> readFile "input/2016/4December.txt"

parseRoom :: String -> Room
parseRoom =
  parse .
  words .
  fmap
    (\x ->
       if x == '-' || x == '['
         then ' '
         else x)
  where
    parse :: [String] -> Room
    parse xs =
      Room
        { rNames = take (length xs - 2) xs
        , rId = read (xs !! (length xs - 2)) :: Int
        , rChecksum = (init . last) xs
        }

isRealRoom :: Room -> Bool
isRealRoom r@Room {rChecksum = rcs} = rcs == calcChecksum r

calcChecksum :: Room -> String
calcChecksum = take 5 . map snd . sortBy comparison . hist
  where
    hist = map (\s@(c:_) -> (length s, c)) . group . sort . concat . rNames
    comparison (l1, c1) (l2, c2) =
      case compare l2 l1 of
        EQ -> compare c1 c2
        x -> x

solution1 :: [Room] -> Int
solution1 = sum . fmap rId . filter isRealRoom

testInput :: [Room]
testInput =
  (fmap parseRoom . lines)
    "aaaaa-bbb-z-y-x-123[abxyz]\n\
\a-b-c-d-e-f-g-h-987[abcde]\n\
\not-a-real-room-404[oarel]\n\
\totally-real-room-200[decoy]"

testSolution1 :: Bool
testSolution1 = solution1 testInput == 1514

fourthDecemberSolution1 :: IO Int
fourthDecemberSolution1 = solution1 <$> input

decrypt :: Room -> String
decrypt r = (unwords . fmap (decryptString (rId r)) . rNames) r
  where
    alphabet :: [Char]
    alphabet = cycle ['a' .. 'z']
    decryptString :: Int -> String -> String
    decryptString _ [] = []
    decryptString shift (x:xs) =
      (dropWhile (x /=) alphabet !! shift) : decryptString shift xs

testSolution2 :: Bool
testSolution2 =
  decrypt (parseRoom "qzmt-zixmtkozy-ivhz-343[notused]") ==
  "very encrypted name"

solution2 :: [Room] -> Int
solution2 =
  snd .
  fromJust .
  find (\(x, _) -> "northpole object storage" == x) .
  fmap (\r -> (decrypt r, rId r))

fourthDecemberSolution2 :: IO Int
fourthDecemberSolution2 = solution2 . filter isRealRoom <$> input
