-------------------------------------------------------------------------------
--                           Advent Of Code - day 2                          --
-------------------------------------------------------------------------------
module SecondDecember
  ( solution1
  , solution2
  ) where

import           Data.Maybe (fromJust, isJust)

data Policy =
  Policy Int Int Char
  deriving (Show)

data Password =
  Password Policy String
  deriving (Show)

input :: IO [Password]
input = do
  content <- readFile "input/2December.txt"
  (return . fmap fromJust . filter isJust . fmap lineToPassword . lines) content

lineToPassword :: String -> Maybe Password
lineToPassword [] = Nothing
lineToPassword l =
  let w = words l
      minAppearence = ((\x -> read x :: Int) . takeWhile ('-' /=)) (head w)
      maxAppearence =
        ((\x -> read x :: Int) . tail . dropWhile ('-' /=)) (head w)
      charPolicy = head $ init (w !! 1)
      password = w !! 2
   in Just $ Password (Policy minAppearence maxAppearence charPolicy) password

validPasswords :: (Password -> Bool) -> [Password] -> Int
validPasswords validation =
  foldr
    (\p v ->
       if validation p
         then v + 1
         else v)
    0

isValidPasswordOld :: Password -> Bool
isValidPasswordOld (Password (Policy min' max' char) password)
  | ((\count -> count >= min' && count <= max') . length . filter (char ==))
     password = True
  | otherwise = False

isValidPasswordNew :: Password -> Bool
isValidPasswordNew (Password (Policy pos1 pos2 char) password)
  | (password !! (pos1 - 1)) == char && (password !! (pos2 - 1)) /= char = True
  | (password !! (pos1 - 1)) /= char && (password !! (pos2 - 1)) == char = True
  | otherwise = False

solution1 :: IO Int
solution1 = validPasswords isValidPasswordOld <$> input

solution2 :: IO Int
solution2 = validPasswords isValidPasswordNew <$> input
