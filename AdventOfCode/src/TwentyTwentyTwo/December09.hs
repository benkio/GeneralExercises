module TwentyTwentyTwo.December09 where

import Data.Set (Set, empty, insert, size)
import Debug.Trace
import Text.Printf

data Move = U Int | D Int | L Int | R Int deriving (Show, Read)

type Position = (Int, Int)

data PlankRope = PlankRope
    { rhead :: Position
    , rtail :: Position
    }
    deriving (Show)

type Rope = [PlankRope]

input :: IO [Move]
input = fmap (\x -> (read x :: Move)) . lines <$> readFile "input/2022/9December.txt"

testInput :: String
testInput =
    "R 4\n\
    \U 4\n\
    \L 3\n\
    \D 1\n\
    \R 4\n\
    \D 1\n\
    \L 5\n\
    \R 2"

testInput2 :: String
testInput2 =
    "R 5\n\
    \U 8\n\
    \L 8\n\
    \D 3\n\
    \R 17\n\
    \D 10\n\
    \L 25\n\
    \U 20"

initialPlankRope :: PlankRope
initialPlankRope = PlankRope{rhead = (0, 0), rtail = (0, 0)}

movePlankRope :: Set Position -> Move -> PlankRope -> (PlankRope, Set Position)
movePlankRope tpos (U 0) r = (r, tpos)
movePlankRope tpos (U m) r@PlankRope{rhead = (hx, hy)} = (\(r', tp) -> movePlankRope (insert tp tpos) (U (m - 1)) r') $ adjustPlankRopeTail $ r{rhead = (hx, hy + 1)}
movePlankRope tpos (D 0) r = (r, tpos)
movePlankRope tpos (D m) r@PlankRope{rhead = (hx, hy)} = (\(r', tp) -> movePlankRope (insert tp tpos) (D (m - 1)) r') $ adjustPlankRopeTail $ r{rhead = (hx, hy - 1)}
movePlankRope tpos (R 0) r = (r, tpos)
movePlankRope tpos (R m) r@PlankRope{rhead = (hx, hy)} = (\(r', tp) -> movePlankRope (insert tp tpos) (R (m - 1)) r') $ adjustPlankRopeTail $ r{rhead = (hx + 1, hy)}
movePlankRope tpos (L 0) r = (r, tpos)
movePlankRope tpos (L m) r@PlankRope{rhead = (hx, hy)} = (\(r', tp) -> movePlankRope (insert tp tpos) (L (m - 1)) r') $ adjustPlankRopeTail $ r{rhead = (hx - 1, hy)}

adjustPlankRopeTail :: PlankRope -> (PlankRope, Position)
adjustPlankRopeTail r@PlankRope{rhead = (hx, hy), rtail = (tx, ty)}
    | diffX == 2 && diffY == 0 = moveTailX r
    | diffX == 0 && diffY == 2 = moveTailY r
    | diffX == 1 && diffY == 2 = moveTailDiagonal r
    | diffX == 2 && diffY == 1 = moveTailDiagonal r
    | diffX == 2 && diffY == 2 = moveTailDiagonal r
    | diffX == 0 && diffY == 0 = (r, rtail r)
    | diffX == 0 && diffY == 1 = (r, rtail r)
    | diffX == 1 && diffY == 0 = (r, rtail r)
    | diffX == 1 && diffY == 1 = (r, rtail r)
    | otherwise = error $ printf "Adjust PlankRope Error: (%d, %d) - (%d, %d)" hx hy tx ty
  where
    diffX = abs (hx - tx)
    diffY = abs (hy - ty)

moveTailX :: PlankRope -> (PlankRope, Position)
moveTailX r@PlankRope{rhead = (hx, _), rtail = (tx, ty)} =
    let rtail' = (tx + mod 1 (hx - tx), ty)
     in (r{rtail = rtail'}, rtail')

moveTailY :: PlankRope -> (PlankRope, Position)
moveTailY r@PlankRope{rhead = (_, hy), rtail = (tx, ty)} =
    let rtail' = (tx, ty + mod 1 (hy - ty))
     in (r{rtail = rtail'}, rtail')

moveTailDiagonal :: PlankRope -> (PlankRope, Position)
moveTailDiagonal r@PlankRope{rhead = (hx, hy), rtail = (tx, ty)} =
    let diffXMod = mod 1 (hx - tx)
        diffYMod = mod 1 (hy - ty)
        rtail' =
            ( tx + if diffXMod == 0 then hx - tx else diffXMod
            , ty + if diffYMod == 0 then hy - ty else diffYMod
            )
     in (r{rtail = rtail'}, rtail')

solution1 :: [Move] -> Int
solution1 = size . snd . foldl (\(r, tset) m -> movePlankRope tset m r) (initialPlankRope, empty)

december09Solution1 :: IO Int
december09Solution1 = solution1 <$> input

initialRope :: Rope
initialRope = replicate 9 initialPlankRope

moveRope :: Set Position -> Move -> Rope -> (Rope, Set Position)
moveRope tpos (U 0) r = (r, tpos)
moveRope tpos (U m) (p : ps) =
    let headMoved = fst $ movePlankRope empty (U 1) p
        tailMoved = adjustRopeTail (rtail headMoved) ps
        rmoved = headMoved : tailMoved
     in moveRope (insert (rtail (last rmoved)) tpos) (U (m - 1)) rmoved
moveRope tpos (D 0) r = (r, tpos)
moveRope tpos (D m) (p : ps) =
    let headMoved = fst $ movePlankRope empty (D 1) p
        tailMoved = adjustRopeTail (rtail headMoved) ps
        rmoved = headMoved : tailMoved
     in moveRope (insert (rtail (last rmoved)) tpos) (D (m - 1)) rmoved
moveRope tpos (R 0) r = (r, tpos)
moveRope tpos (R m) (p : ps) =
    let headMoved = fst $ movePlankRope empty (R 1) p
        tailMoved = adjustRopeTail (rtail headMoved) ps
        rmoved = headMoved : tailMoved
     in moveRope (insert (rtail (last rmoved)) tpos) (R (m - 1)) rmoved
moveRope tpos (L 0) r = (r, tpos)
moveRope tpos (L m) (p : ps) =
    let headMoved = fst $ movePlankRope empty (L 1) p
        tailMoved = adjustRopeTail (rtail headMoved) ps
        rmoved = headMoved : tailMoved
     in moveRope (insert (rtail (last rmoved)) tpos) (L (m - 1)) rmoved

adjustRopeTail :: Position -> Rope -> Rope
adjustRopeTail _ [] = []
adjustRopeTail prevTail (r : rs) =
    let (pr, prt) = adjustPlankRopeTail (r{rhead = prevTail})
     in pr : adjustRopeTail prt rs

solution2 :: [Move] -> Int
solution2 = size . snd . foldl (\(r, tset) m -> moveRope tset m r) (initialRope, empty)

december09Solution2 :: IO Int
december09Solution2 = solution2 <$> input
