module TwentyTwentyOne.FourteenthDecember where

import Data.List (group, sort)
import Data.Map (Map, (!))
import qualified Data.Map as M (elems, empty, foldlWithKey, fromList, insertWith, lookup)

parseInput :: String -> (String, Map String Char)
parseInput =
  ( \xs ->
      ( head xs,
        (M.fromList . fmap (\x -> (take 2 x, last x)) . drop 2) xs
      )
  )
    . lines

polimerGeneration :: String -> Map String Char -> String
polimerGeneration [] _ = []
polimerGeneration [x] _ = [x]
polimerGeneration (x : y : xs) ts = foldl (\_ l -> [x, l]) [x, y] searchTransformation ++ polimerGeneration (y : xs) ts
  where
    searchTransformation = M.lookup [x, y] ts

multiStepGen :: Int -> String -> Map String Char -> String
multiStepGen step xs ts = iterate (`polimerGeneration` ts) xs !! step

input :: IO String
input = readFile "input/2021/14December.txt"

inputTest :: String
inputTest =
  "NNCB\n\
  \\n\
  \CH -> B\n\
  \HH -> N\n\
  \CB -> H\n\
  \NH -> C\n\
  \HB -> C\n\
  \HC -> B\n\
  \HN -> C\n\
  \NN -> C\n\
  \BH -> H\n\
  \NC -> B\n\
  \NB -> B\n\
  \BN -> B\n\
  \BB -> N\n\
  \BC -> B\n\
  \CC -> N\n\
  \CN -> C"

solution1 :: String -> Int
solution1 = (\xs -> maximum xs - minimum xs) . fmap length . group . sort . uncurry (multiStepGen 10) . parseInput

fourteenthDecemberSolution1 :: IO Int
fourteenthDecemberSolution1 = solution1 <$> input

polimerToMaps :: String -> Map String Char -> (Map Char Int, Map String Int)
polimerToMaps p ts =
  ( -- 1 - insert all the char of c in cs
    foldl (\ms v -> M.insertWith (+) v 1 ms) M.empty p,
    foldl
      ( \ys c ->
          -- 2 - if c is in ts then upsert c as key and value + 1
          foldl (\_ _ -> M.insertWith (+) c 1 ys) ys (M.lookup c ts)
      )
      M.empty
      (zipWith (\x y -> [x, y]) p (tail p))
  )

polimerGeneration' :: Map Char Int -> Map String Int -> Map String Char -> (Map Char Int, Map String Int)
polimerGeneration' cs ms ts =
  M.foldlWithKey
    ( \(xs, ys) c v ->
        -- find next char(nxt) in ts
        -- add to xs the nxt -> oldv + v
        -- Generate the two new combinations (n1, n2)
        -- add n1 -> v, n2 -> v in ys
        let nxt = ts ! c
            xs' = M.insertWith (+) nxt v xs
            (n1, n2) = ([head c, nxt], [nxt, last c])
            ys' = M.insertWith (+) n1 v ys
            ys'' = M.insertWith (+) n2 v ys'
         in (xs', ys'')
    )
    (cs, M.empty)
    ms

multiStepGen' :: Int -> Map String Char -> Map Char Int -> Map String Int -> (Map Char Int, Map String Int)
multiStepGen' step ts cs ms = iterate (\(cs', ms') -> polimerGeneration' cs' ms' ts) (cs, ms) !! step

solution2 :: String -> Int
solution2 =
  (\m -> maximum m - minimum m)
    . M.elems
    . fst
    . (\(i, ts) -> uncurry (multiStepGen' 40 ts) $ polimerToMaps i ts)
    . parseInput

fourteenthDecemberSolution2 :: IO Int
fourteenthDecemberSolution2 = solution2 <$> input
