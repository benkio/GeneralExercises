-------------------------------------------------------------------------------
--                           Advent Of Code - day 13                          --
-------------------------------------------------------------------------------
module TwentyTwenty.ThirteenthDecember where

import Control.Monad (zipWithM)
import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.List (minimumBy)
import Data.Maybe (fromJust, isJust)

input :: IO (Int, [String])
input = do
    content <- readFile "input/2020/13December.txt"
    let departTime = read (head (lines content)) :: Int
        busses =
            filter
                (not . null)
                ( fst
                    ( foldl
                        ( \(acc, b) c ->
                            case c of
                                ',' -> (acc ++ [b], [])
                                _ -> (acc, b ++ [c])
                        )
                        ([[]], [])
                        (lines content !! 1)
                    )
                )
    return (departTime, busses)

bussesTimelines :: [Int] -> [[Int]]
bussesTimelines = fmap (\x -> iterate (+ x) x)

findMyBusNWait :: Int -> [[Int]] -> (Int, Int)
findMyBusNWait myTimestamp =
    (\(b, t) -> (b, t - myTimestamp))
        . minimumBy (\(_, a) (_, b) -> compare a b)
        . foldl (\acc bt -> acc ++ [(head bt, head (dropWhile (myTimestamp >) bt))]) []

thirteenthDecemberSolution1 :: IO Int
thirteenthDecemberSolution1 =
    uncurry (*)
        . uncurry findMyBusNWait
        . second (bussesTimelines . fmap (\x -> read x :: Int) . filter ("x" /=))
        <$> input

-----------------------------------------------------------------------------
--                          Copied from the web :(                          --
-----------------------------------------------------------------------------

-- IDs paired with offsets
parse :: String -> Int -> [(Int, Int)]
parse [] _ = []
parse xs offset
    | head xs == 'x' = parse (tail xs) (offset + 1)
    | head xs == ',' = parse (tail xs) offset
    | otherwise =
        let (n, xs') = span isDigit xs
         in (read n :: Int, offset) : parse xs' (offset + 1)

task1 ts = head . concatMap (filter isJust . (\(ts, ids) -> map (check ts) ids))
  where
    check ts' id
        | ts' `mod` id == 0 = Just ((ts' - ts) * id)
        | otherwise = Nothing

-- Chinese Remainder Gaussian
-- https://en.wikipedia.org/wiki/Chinese_remainder_theorem
crt :: [Int] -> Int -> [Int] -> Int
crt diffs mprod ids =
    let ins = zip diffs ids
     in foldr (\(x, y) r -> r + aux x y) 0 ins `mod` mprod
  where
    aux x y =
        let f = (mprod `div` y) `inv` y
         in ((x * mprod) `div` y) * f
    -- Modular multiplicative inverse
    -- https://en.wikipedia.org/wiki/Modular_multiplicative_inverse
    a `inv` m = let (_, i, _) = gcd' a m in i `mod` m
    -- Extended Euclidean Algorithm
    -- stack overflow
    -- (https://stackoverflow.com/questions/35529211/chinese-remainder-theorem-haskell)
    gcd' 0 b = (b, 0, 1)
    gcd' a b = (g, t - (b `div` a) * s, s)
      where
        (g, s, t) = gcd' (b `mod` a) a

thirteenthDecemberSolution2 :: IO Int
thirteenthDecemberSolution2 = do
    [_, ids] <- lines <$> readFile "input/2020/13December.txt"
    let ids' = parse ids 0
        ids'' = map fst ids'
    return $ crt (map (uncurry (-)) ids') (product ids'') ids''
