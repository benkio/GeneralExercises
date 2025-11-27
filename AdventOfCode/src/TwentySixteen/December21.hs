module TwentySixteen.December21 where

import Data.Bifunctor
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Sequence (Seq, deleteAt, elemIndexL, fromList, index, insertAt, update, (><))
import qualified Data.Sequence as Seq (reverse, splitAt)

data Direction = L | R deriving (Show)

data Instruction
    = SwapPosition Int Int
    | SwapLetter Char Char
    | RotatePosition Char
    | Rotate Direction Int
    | ReversePosition Int Int
    | MovePosition Int Int
    deriving (Show)

input :: IO [Instruction]
input = fmap parseInstruction . lines <$> readFile "input/2016/21December.txt"

parseInstruction :: String -> Instruction
parseInstruction s
    | "swap position " `isPrefixOf` s = ((\l -> SwapPosition (read (head l) :: Int) (read (last l) :: Int)) . words . fromJust . stripPrefix "swap position ") s
    | "swap letter " `isPrefixOf` s = ((\l -> SwapLetter (head (head l)) (head (last l))) . words . fromJust . stripPrefix "swap letter ") s
    | "rotate based on position of letter " `isPrefixOf` s = (RotatePosition . head . fromJust . stripPrefix "rotate based on position of letter ") s
    | "rotate " `isPrefixOf` s = ((\l -> Rotate (if head l == "left" then L else R) (read (l !! 1) :: Int)) . words . fromJust . stripPrefix "rotate ") s
    | "reverse positions " `isPrefixOf` s = ((\l -> ReversePosition (read (head l) :: Int) (read (last l) :: Int)) . words . fromJust . stripPrefix "reverse positions ") s
    | "move position " `isPrefixOf` s = ((\l -> MovePosition (read (head l) :: Int) (read (last l) :: Int)) . words . fromJust . stripPrefix "move position ") s

applyInstruction :: Seq Char -> Instruction -> Seq Char
applyInstruction s (SwapPosition x y) =
    let a = index s x
        b = index s y
     in (update y a . update x b) s
applyInstruction s (SwapLetter a b) =
    let ai = (fromJust . elemIndexL a) s
        bi = (fromJust . elemIndexL b) s
     in applyInstruction s (SwapPosition ai bi)
applyInstruction s (ReversePosition x y) =
    let (start, (reversed, rest)) = (second (first Seq.reverse . Seq.splitAt (y - x + 1)) . Seq.splitAt x) s
     in start >< reversed >< rest
applyInstruction s (Rotate L x) = (fromList . take (length s) . drop x . cycle . toList) s
applyInstruction s (Rotate R x) = (fromList . take (length s) . drop toDrop . cycle . toList) s
  where
    toDrop = length s - (x `mod` length s)
applyInstruction s (MovePosition x y) = (insertAt y letter . deleteAt x) s
  where
    letter = index s x
applyInstruction s (RotatePosition a) =
    let ai = (fromJust . elemIndexL a) s
        plus = if ai >= 4 then 1 else 0
     in applyInstruction s (Rotate R (1 + ai + plus))

testInput :: [Instruction]
testInput =
    (fmap parseInstruction . lines)
        "swap position 4 with position 0\n\
        \swap letter d with letter b\n\
        \reverse positions 0 through 4\n\
        \rotate left 1 step\n\
        \move position 1 to position 4\n\
        \move position 3 to position 0\n\
        \rotate based on position of letter b\n\
        \rotate based on position of letter d"

solution1 :: String -> [Instruction] -> String
solution1 s is = toList $ foldl applyInstruction (fromList s) is

testSolution1 :: Bool
testSolution1 = solution1 "abcde" testInput == "decab"

december21Solution1 :: IO String
december21Solution1 = solution1 "abcdefgh" <$> input

solution2 :: String -> [Instruction] -> String
solution2 s is = toList $ foldl unapplyInstruction (fromList s) (reverse is)

unapplyInstruction :: Seq Char -> Instruction -> Seq Char
unapplyInstruction s (Rotate R x) = (fromList . take (length s) . drop x . cycle . toList) s
unapplyInstruction s (Rotate L x) = (fromList . take (length s) . drop toDrop . cycle . toList) s
  where
    toDrop = length s - (x `mod` length s)
unapplyInstruction s (SwapPosition x y) =
    let a = index s x
        b = index s y
     in (update y a . update x b) s
unapplyInstruction s (SwapLetter a b) =
    let ai = (fromJust . elemIndexL a) s
        bi = (fromJust . elemIndexL b) s
     in unapplyInstruction s (SwapPosition ai bi)
unapplyInstruction s (ReversePosition x y) =
    let (start, (reversed, rest)) = (second (first Seq.reverse . Seq.splitAt (y - x + 1)) . Seq.splitAt x) s
     in start >< reversed >< rest
unapplyInstruction s (MovePosition x y) = (insertAt x letter . deleteAt y) s
  where
    letter = index s y
unapplyInstruction s (RotatePosition a) =
    let ai = (fromJust . elemIndexL a) s
        result = ai `div` 2 + (if odd ai || ai == 0 then 1 else 5)
     in applyInstruction s (Rotate L result)

testSolution2 :: Bool
testSolution2 = solution2 "gbhafcde" testInput == "abcdefgh"

test :: IO Bool
test = do
    is <- input
    let s = fromList "abcdefgh"
        s' = fromList "gbhafcde"
        encrypt = scanl applyInstruction s is
        decrypt = scanl unapplyInstruction s' (reverse is)
    print (zip3 (reverse encrypt) decrypt (reverse is))
    return $ show encrypt == show (reverse decrypt)

december21Solution2 :: IO String
december21Solution2 = solution2 "fbgdceah" <$> input
