module TwentySixteen.NinthDecember where

import Data.List

input :: IO String
input = init <$> readFile "input/2016/9December.txt"

solution1 :: String -> Int
solution1 [] = 0
solution1 s@(x:xs)
  | x == '(' =
    let (decompressedString, rest) = decompressString s
    in length decompressedString + solution1 rest
  | otherwise = 1 + solution1 xs

decompressString :: String -> (String, String)
decompressString s =
  let (count, repetitions, s') = parseCompressPattern s
  in (
    (concat . replicate repetitions) (take count s')
    ,drop count s'
     )

parseCompressPattern :: String -> (Int, Int, String)
parseCompressPattern s = (
  ((\x -> read x :: Int) . takeWhile ('x' /=)) (tail s)
  ,((\x -> read x :: Int) . takeWhile (')' /=) . tail . dropWhile ('x' /=)) (tail s)
  ,(tail . dropWhile (')' /=)) s
                        )

ninthDecemberSolution1 :: IO Int
ninthDecemberSolution1 = solution1 <$> input

solution2 :: String -> Int
solution2 = undefined

ninthDecemberSolution2 :: IO Int
ninthDecemberSolution2 = undefined
