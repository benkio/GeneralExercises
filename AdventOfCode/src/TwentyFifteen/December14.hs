module TwentyFifteen.December14 where

import Data.List

data Rendeer = Rendeer
    { name :: String
    , speed :: Int
    , stamina :: Int
    , rest :: Int
    }
    deriving (Show)

input :: IO [Rendeer]
input = fmap parseRendeer . lines <$> readFile "input/2015/14December.txt"

parseRendeer :: String -> Rendeer
parseRendeer =
    ( \ws ->
        Rendeer
            { name = head ws
            , speed = read (ws !! 3) :: Int
            , stamina = read (ws !! 6) :: Int
            , rest = read (ws !! (length ws - 2)) :: Int
            }
    )
        . words

inputTest :: [Rendeer]
inputTest =
    (fmap parseRendeer . lines)
        "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\n\
        \Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."

rendeerRace1 :: Rendeer -> [Int]
rendeerRace1 Rendeer{speed = rsp, stamina = rst, rest = rr} =
    scanl (secondDistanceStep rst rr rsp) 0 [1 ..]

secondDistanceStep :: Int -> Int -> Int -> Int -> Int -> Int
secondDistanceStep rst rr rsp distance sec
    | isRestTime (cycle [rst, rr]) rr sec = distance
    | otherwise = distance + rsp

isRestTime :: [Int] -> Int -> Int -> Bool
isRestTime (r : rs) rest sec
    | (sec - r) <= 0 && r == rest = True
    | (sec - r) <= 0 && r /= rest = False
    | otherwise = isRestTime rs rest (sec - r)

solution1 :: Int -> [Rendeer] -> Int
solution1 sec = maximum . fmap (\x -> rendeerRace1 x !! sec)

solution1Test :: Bool
solution1Test = solution1 1000 inputTest == 1120

december14Solution1 :: IO Int
december14Solution1 = solution1 2503 <$> input

rendeerRace2 :: [Rendeer] -> [[Int]]
rendeerRace2 rs =
    ( scanl calculateLeader (fmap (const 0) rs)
        . transpose
        . fmap (tail . rendeerRace1)
    )
        rs

calculateLeader :: [Int] -> [Int] -> [Int]
calculateLeader currentPoints rendeerDistances =
    let currentPointsIndexed = [0 ..] `zip` currentPoints
        rendeerDistancesIndexed = [0 ..] `zip` rendeerDistances
        (leaderIndex, _) =
            maximumBy (\(_, x) (_, y) -> compare x y) rendeerDistancesIndexed
     in fmap
            ( \(i, p) ->
                if i == leaderIndex
                    then p + 1
                    else p
            )
            currentPointsIndexed

solution2 :: Int -> [Rendeer] -> Int
solution2 sec = maximum . (!! sec) . rendeerRace2

solution2Test :: Bool
solution2Test = solution2 1000 inputTest == 689

december14Solution2 :: IO Int
december14Solution2 = solution2 2503 <$> input
