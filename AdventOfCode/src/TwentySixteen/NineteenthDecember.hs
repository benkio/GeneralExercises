module TwentySixteen.NineteenthDecember where

import           Data.Functor

data Elve = Elve {gifts :: Int, nextElveDistance :: Int} deriving (Show)

input :: IO Int
input = (\x -> read x :: Int) . init <$> readFile "input/2016/19December.txt"

elves :: Int -> [Elve]
elves num =
  let (x, y) = num `divMod` 2
   in replicate y (Elve {gifts = 1, nextElveDistance = 1}) ++ replicate x (Elve {gifts = 2, nextElveDistance = 2})

initialIndex :: Int -> Int
initialIndex x
  | even x = 0
  | odd x = x - 1

-- solution1 :: Int -> [Elve] -> Int -> IO Int
-- solution1 index (e@Elve {gifts = g, nextElveDistance = d} : es) target
--   | g == target = return $ index + 1
--   | otherwise =
--     let e' = e {gifts = g + gifts (head es), nextElveDistance = d + nextElveDistance (head es)}
--         nextIndex = (index + nextElveDistance e') `mod` target
--         elves' = (tail es) ++ [e']
--      in do
--           putStrLn ("new Elves: " ++ (show e') ++ " index: " ++ show index ++ " nextIndex: " ++ show nextIndex)
--           solution1 nextIndex elves' target

solution1Test :: IO Bool
solution1Test = solution1 (initialIndex 5) (elves 5) 5 <&> (== 3)

nineteenthDecemberSolution1 :: IO Int
nineteenthDecemberSolution1 = input >>= (\x -> solution1 (initialIndex x) (elves x) x)

solution2 :: String -> Int
solution2 = undefined

nineteenthDecemberSolution2 :: IO Int
nineteenthDecemberSolution2 = undefined
