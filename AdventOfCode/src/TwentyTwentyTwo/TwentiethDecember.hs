module TwentyTwentyTwo.TwentiethDecember where

import Data.IntMap (IntMap, filterWithKey, fromList, toList, union, (!))
import qualified Data.IntMap as M (filter, foldl, map)
import Data.Maybe (fromJust)
import Debug.Trace

data Node = Node
    { eid :: Int
    , left :: Int
    , right :: Int
    , value :: Int
    }
    deriving (Eq, Show)

-- instance Show Node where
--     show = show . value

findEid i ns = ns ! i
findZero ns = snd . head . toList $ M.filter ((== 0) . value) ns
nodeAtDistanceRight n 0 _ = n
nodeAtDistanceRight n d ns = nodeAtDistanceRight nr (d - 1) ns
  where
    nr = findEid (right n) ns
nodeAtDistanceLeft n 0 _ = n
nodeAtDistanceLeft n d ns = nodeAtDistanceLeft nl (d + 1) ns
  where
    nl = findEid (left n) ns
constrainIndex i ns = (signum i) * (mod (abs i) (length ns - 1))
stepsToMove n ns = constrainIndex (value n) ns
extractNode n ns = ns'
  where
    nl = findEid (left n) ns
    nr = findEid (right n) ns
    nl' = nl{right = eid nr}
    nr' = nr{left = eid nl}
    ns' = filterWithKey (\k _ -> k /= (eid n)) $ union (fromList [(eid nl', nl'), (eid nr', nr')]) ns -- [nl', nr'] ++ (filter ((`notElem` (fmap eid [nl, nr, n])) . eid) ns)

insertNodeToRight :: Node -> Node -> IntMap Node -> IntMap Node
insertNodeToRight n t ns = ns'
  where
    tr = findEid (right t) ns
    tl = findEid (eid t) ns
    tl' = tl{right = eid n}
    tr' = tr{left = eid n}
    n' = n{left = eid tl', right = eid tr'}
    ns' = union (fromList [(eid tl', tl'), (eid tr', tr'), (eid n', n')]) ns -- [tl', tr', n'] ++ (filter ((`notElem` (fmap eid [tl, tr, n])) . eid) ns)

insertNodeToLeft :: Node -> Node -> IntMap Node -> IntMap Node
insertNodeToLeft n t ns = ns'
  where
    tl = findEid (left t) ns
    tr = findEid (eid t) ns
    tr' = tr{left = eid n}
    tl' = tl{right = eid n}
    n' = n{left = eid tl', right = eid tr'}
    ns' = union (fromList [(eid tl', tl'), (eid tr', tr'), (eid n', n')]) ns --[tl', tr', n'] ++ (filter ((`notElem` (fmap eid [tl, tr, n])) . eid) ns)

moveNode :: Node -> IntMap Node -> IntMap Node
moveNode n ns
    | steps == 0 = ns
    | steps < 0 = insertNodeToLeft n target ns'
    | steps > 0 = insertNodeToRight n target ns'
  where
    steps = stepsToMove n ns
    target
        | steps > 0 = nodeAtDistanceRight n steps ns
        | steps < 0 = nodeAtDistanceLeft n steps ns
    ns' = extractNode n ns

-- printns ns = printll (head ns) (length ns) ns
-- printll _ 0 _ = ""
-- printll n x ns = ((show . value) n) ++ " " ++ printll (nodeAtDistanceRight n 1 ns) (x-1) ns

mix :: IntMap Node -> IntMap Node -> IntMap Node
mix sequence ns =
    M.foldl
        ( \acc n ->
            let n' = findEid (eid n) acc
                xs = moveNode n' acc
             in xs
        )
        ns
        sequence

findCoordinates ns = fmap (\s -> value (nodeAtDistanceRight (findZero ns) s ns)) [1000, 2000, 3000]

solution ns = (sum . findCoordinates . mix ns) ns

input :: IO (IntMap Node)
input = parseInput <$> readFile "input/2022/20December.txt"

testInput :: IntMap Node
testInput =
    parseInput
        "1\n\
        \2\n\
        \-3\n\
        \3\n\
        \-2\n\
        \0\n\
        \4"

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution <$> input

decriptionKey = 811589153

solution2 ns =
    ( sum
        . findCoordinates
        . snd
    )
        ( until
            (\(r, _) -> r == 10)
            ( \(r, xs) ->
                (r + 1, mix nsD xs)
            )
            (0, nsD)
        )
  where
    nsD = M.map (\n -> n{value = value n * decriptionKey}) ns

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = solution2 <$> input

parseInput input =
    ( fromList
        . fmap (uncurry buildNode)
        . ([0 ..] `zip`)
        . lines
    )
        input
  where
    total = (length . lines) input
    buildNode i v
        | i == 0 = (i, Node{eid = i, left = total - 1, right = i + 1, value = (read v :: Int)})
        | i == total - 1 = (i, Node{eid = i, left = i - 1, right = 0, value = (read v :: Int)})
        | otherwise = (i, Node{eid = i, left = i - 1, right = i + 1, value = (read v :: Int)})
