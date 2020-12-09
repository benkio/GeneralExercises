-------------------------------------------------------------------------------
--                           Advent Of Code - day 7                          --
-------------------------------------------------------------------------------
module TwentyTwenty.SeventhDecember where

import Data.List (find)
import Data.Maybe (fromJust)
import Text.Regex.TDFA

data Bag =
  Bag String [Bag]
  deriving (Show)

bagName :: Bag -> String
bagName (Bag s _) = s

bagContent :: Bag -> [Bag]
bagContent (Bag _ bs) = bs

input :: IO [Bag]
input =
  parseBags [] . fmap bagRegexMatch . lines <$> readFile "input/2020/7December.txt"

bagRegexMatch :: String -> (String, [String])
bagRegexMatch i =
  let (bName, _, bContent') = i =~ "contain" :: (String, String, String)
      bContent =
        (\x -> take (length x - 4) x) <$>
        getAllTextMatches (bContent' =~ "[0-9][a-z ]* bag") :: [String]
   in (take (length bName - 6) bName, bContent)

parseBags :: [Bag] -> [(String, [String])] -> [Bag]
parseBags bags [] = bags
parseBags bags (b:bs)
  | null currentBagContent = parseBags (Bag (fst b) [] : bags) bs
  | all (`elem` bagNames) (fmap snd currentBagContent) =
    let newBagContent =
          currentBagContent >>=
          (\x -> replicate (fst x) (fromJust (find ((snd x ==) . bagName) bags)))
     in parseBags (Bag (fst b) newBagContent : bags) bs
  | otherwise = parseBags bags (bs ++ [b])
  where
    bagNames = fmap bagName bags
    currentBagContent = fmap (\x -> (read [head x] :: Int, drop 2 x)) (snd b)

isBagContained :: String -> Bag -> Bool
isBagContained bName (Bag bn bc)
  | bn == bName = True
  | bn /= bName && null bc = False
  | otherwise = or $ isBagContained bName <$> bc

searchForBagInContent :: String -> [Bag] -> [Bag]
searchForBagInContent bName = filter (isBagContained bName)

countContainedBags :: Bag -> Int
countContainedBags = foldl (\x bc -> x + countContainedBags bc) 1 . bagContent

seventhDecemberSolution1 :: IO Int
seventhDecemberSolution1 =
  (\x -> x - 1) . length . searchForBagInContent "shiny gold" <$> input

seventhDecemberSolution2 :: IO Int
seventhDecemberSolution2 =
  (\x -> x - 1) .
  countContainedBags . fromJust . find (("shiny gold" ==) . bagName) <$>
  input
