module TwentySixteen.NineteenthDecember where

import Data.Functor
import Data.Sequence (Seq, deleteAt, fromList, (|>))
import qualified Data.Sequence as Seq (index, length)
import System.IO
import Text.Printf

input :: IO Int
input = (\x -> read x :: Int) . init <$> readFile "input/2016/19December.txt"

elves :: Int -> (Int, [Int])
elves m = (m, [1, 3 .. m])

solution1 :: (Int -> Bool) -> (Int, [Int]) -> Int
solution1 _ (_, [x]) = x
solution1 f (prevGenLength, elv) =
  solution1 nextFilter (length elv, (fmap fst . filter (nextFilter . snd) . (`zip` [1 ..])) elv)
  where
    nextFilter = if even prevGenLength then f else not . f

solution1Test :: Bool
solution1Test = solution1 odd (elves 5) == 3

nineteenthDecemberSolution1 :: IO Int
nineteenthDecemberSolution1 = solution1 odd . elves <$> input

solution2 :: Seq Int -> Int
solution2 l
  | Seq.length l == 1 = Seq.index l 0
  | otherwise =
    let deleteIndex = Seq.length l `div` 2
        h = Seq.index l 0
        l' = ((|> h) . deleteAt 0 . deleteAt deleteIndex) l
     in solution2 l'

solution2Test :: Bool
solution2Test = solution2 (fromList [1 .. 5]) == 2

nineteenthDecemberSolution2 :: IO Int
nineteenthDecemberSolution2 = input <&> solution2 . (\x -> fromList [1 .. x])
