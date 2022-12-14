module TwentyTwentyTwo.TwelfthDecember where

import Data.Bifunctor (second)
import Data.List (find, nub, sortOn, (\\))
import Data.Map (Map, elems, findMax, fromList, insert, keys, lookup, (!))
import Data.Maybe (catMaybes, fromJust, isJust)
import Debug.Trace
import Text.Printf
import Prelude hiding (lookup)

type Position = (Int, Int)

data Ground = Ground
    { position :: Position
    , height :: Char
    }
    deriving (Eq)

instance Show Ground where
    show (Ground{position = p, height = h}) = printf "%s -> %c" (show p) h

input :: IO (Map Position Ground)
input = toGrid <$> readFile "input/2022/12December.txt"

toGrid :: String -> Map Position Ground
toGrid = fromList . concatMap (fmap amendStartEnd . uncurry parseRow) . (`zip` [0 ..]) . lines
  where
    parseRow row y = (fmap (\(c, x) -> Ground{position = (x, y), height = c}) . (`zip` [0 ..])) row
    amendStartEnd g@(Ground{height = 'S'}) = (position g, g{height = '`'})
    amendStartEnd g@(Ground{height = 'E'}) = (position g, g{height = '{'})
    amendStartEnd g = (position g, g)

neighboor :: Map Position Ground -> Ground -> [Ground]
neighboor m (Ground{position = (x, y), height = h}) =
    ( filter (\x -> height x <= succ h)
        . catMaybes
        . fmap (flip lookup m)
    )
        [(x - 1, y), (x + 1, y), (x, y + 1), (x, y - 1)]

knownGround :: Map Position [Ground] -> [[Ground]] -> Map Position [Ground]
knownGround = foldl addToKnown
  where
    addToKnown m xs =
        let lastP = (position . last) xs
            mayBestSoFar = lookup lastP m
         in maybe
                (insert lastP xs m)
                ( \bs ->
                    if length bs >= length xs then insert lastP xs m else m
                )
                mayBestSoFar

explode :: Map Position Ground -> Map Position [Ground] -> [Position] -> (Map Position [Ground], [Position])
explode m km lastKeys = (knownGround km closestPathsExtended, ks)
  where
    closestKeys = ((\\ lastKeys) . keys) km
    ks = if null closestKeys then keys km else closestKeys
    closestPathsExtended = concatMap (extendPath m km) $ ks

extendPath :: Map Position Ground -> Map Position [Ground] -> Position -> [[Ground]]
extendPath m km k =
    let path = km ! k
        ns = neighboor m (m ! k)
     in fmap (\n -> path ++ [n]) ns

gridPaths :: Ground -> Map Position Ground -> Position -> Map Position [Ground]
gridPaths start m target =
    let known = fromList [(position start, [start])]
     in fst $ until (exitCondition target . fst) (uncurry (explode m)) (known, [])

solution1 :: Map Position Ground -> Int
solution1 m =
    let start = (fromJust . find ((== '`') . height) . elems) m
        target = (position . fromJust . find ((== '{') . height) . elems) m
        ex = gridPaths start m target
     in (\x -> x - 1) $ length $ ex ! target

exitCondition :: Position -> Map Position [Ground] -> Bool
exitCondition t = isJust . lookup t

testInput :: Map Position Ground
testInput =
    toGrid
        "Sabqponm\n\
        \abcryxxl\n\
        \accszExk\n\
        \acctuvwj\n\
        \abdefghi"

solution2 :: Map Position Ground -> Int
solution2 m =
    let starts = (filter (\g -> height g == '`' || height g == 'a') . elems) m
        target = (position . fromJust . find ((== '{') . height) . elems) m
        exs = fmap (\s -> gridPaths s m target) starts
     in ((\x -> x - 1) . minimum) $ fmap (length . (! target)) exs

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = solution1 <$> input

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = solution2 <$> input
