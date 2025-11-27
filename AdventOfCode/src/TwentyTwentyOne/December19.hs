module TwentyTwentyOne.December19 where

import Data.Bifunctor (second)
import Data.List (find, groupBy, nub, transpose)
import Data.Map (Map)
import qualified Data.Map as M (filter, fromList, map, toList)
import Data.Maybe (fromJust, isJust)
import Debug.Trace

type Coord = (Int, Int, Int)

data Scanner = Scanner Int [Coord] deriving (Show, Eq, Ord)

bacons :: Scanner -> [Coord]
bacons (Scanner _ bs) = bs

scannerId :: Scanner -> Int
scannerId (Scanner x _) = x

changePerspective :: [Coord] -> [(Int, [Coord])]
changePerspective bs =
    [0 ..]
        `zip` transpose
            ( fmap
                ( \(bx, by, bz) ->
                    [ v
                    | x <- [bx, -bx]
                    , y <- [by, -by]
                    , z <- [bz, -bz]
                    , -- definitely too much
                    v <- [(x, y, z), (y, x, z), (z, y, x), (y, z, x), (z, x, y), (x, z, y)] -- [(x, y, z), (y, z, x), (z, x, y), ]
                    ]
                )
                bs
            )

parseInput :: String -> [Scanner]
parseInput =
    fmap (uncurry Scanner)
        . zip [0 ..]
        . fmap
            ( fmap parseCoordinate
                . filter (',' `elem`)
            )
        . groupBy (\_ s' -> s' /= "")
        . lines

parseCoordinate :: String -> Coord
parseCoordinate s =
    let c1 = takeWhile (/= ',') s
        c2 = (takeWhile (/= ',') . tail . dropWhile (/= ',')) s
        c3 = (tail . dropWhile (/= ',') . tail . dropWhile (/= ',')) s
     in (read c1 :: Int, read c2 :: Int, read c3 :: Int)

baconPov :: [Coord] -> Map Coord [Coord]
baconPov bs = M.fromList $ buildPov bs bs
  where
    buildPov [] _ = []
    buildPov (b@(bx, by, bz) : bs') bs = (b, filter (/= (0, 0, 0)) $ [(bx - bx', by - by', bz - bz') | (bx', by', bz') <- bs]) : buildPov bs' bs

matchBacon :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Bool
matchBacon b b' = any (\(_, xs) -> checkBaconPerspective 0 b xs) $ changePerspective b'

checkBaconPerspective :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Bool
checkBaconPerspective c [] _ = c >= 11
checkBaconPerspective c (y : ys) zs
    | c == 11 = True
    | y `elem` zs = checkBaconPerspective (c + 1) ys zs
    | otherwise = checkBaconPerspective c ys zs

matchScanner :: Scanner -> Scanner -> Maybe (Coord, Int, [(Coord, Coord)])
matchScanner (Scanner _ bs) (Scanner _ bs') =
    let bsr = baconPov bs
        bsr' = baconPov bs'
        searchList = M.toList $ M.map fromJust $ M.filter isJust $ M.map (searchInScanner (M.toList bsr')) bsr
        (indexTransformation, scannerPosition) = calculateScannerPosition searchList
     in if length searchList >= 12
            then Just (scannerPosition, indexTransformation, searchList)
            else Nothing

mergeScanners :: Scanner -> Scanner -> Coord -> Int -> Scanner
mergeScanners (Scanner i bs) (Scanner _ bs') (x', y', z') transformationIndex =
    Scanner i (nub (bs ++ fmap (\(x, y, z) -> (x' - x, y' - y, z' - z)) (snd (changePerspective bs' !! transformationIndex))))

relativeScannerPositions :: [Coord] -> Scanner -> [Scanner] -> [Coord]
relativeScannerPositions cs _ [] = (0, 0, 0) : cs
relativeScannerPositions cs s (x : xs) = case -- traceShow (scannerId s, (length . bacons) s, fmap scannerId (x : xs)) 
    matchScanner s x of
    Just (scannerRelativePos, transformationIndex, _) -> relativeScannerPositions (cs ++ [scannerRelativePos]) (mergeScanners s x scannerRelativePos transformationIndex) xs
    Nothing -> relativeScannerPositions cs s (xs ++ [x])

mergeAllScanners :: Scanner -> [Scanner] -> Scanner
mergeAllScanners s [] = s
mergeAllScanners s (x : xs) = case -- traceShow (scannerId s, (length . bacons) s, fmap scannerId (x : xs)) 
    matchScanner s x of
    Just (scannerRelativePos, transformationIndex, _) -> mergeAllScanners (mergeScanners s x scannerRelativePos transformationIndex) xs
    Nothing -> mergeAllScanners s (xs ++ [x])

buildPairs [] = []
buildPairs (s : ss) = [(s, x) | x <- ss] ++ buildPairs ss

calculateScannerPosition :: [(Coord, Coord)] -> (Int, Coord)
calculateScannerPosition =
    second head
        . fromJust
        . find (\(i, xs) -> all (uncurry (==)) (xs `zip` tail xs))
        . ( \(b, b') ->
                ( \(i, xs) ->
                    ( i
                    , (\((x, y, z), (x', y', z')) -> (x + x', y + y', z + z'))
                        <$> b `zip` xs
                    )
                )
                    <$> changePerspective b'
          )
        . unzip

searchInScanner :: [((Int, Int, Int), [(Int, Int, Int)])] -> [(Int, Int, Int)] -> Maybe (Int, Int, Int)
searchInScanner [] sbs = Nothing
searchInScanner ((k, bs) : bs') sbs =
    if matchBacon sbs bs then Just k else searchInScanner bs' sbs

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance (x, y, z) (x', y', z') = abs (x - x') + abs (y - y') + abs (z - z')

solution2 :: [Scanner] -> Int
solution2 ss =
    let cs = relativeScannerPositions [] (head ss) (tail ss)
     in maximum $ uncurry manhattanDistance <$> buildPairs cs

inputTest :: String
inputTest =
    "--- scanner 0 ---\n\
    \404,-588,-901\n\
    \528,-643,409\n\
    \-838,591,734\n\
    \390,-675,-793\n\
    \-537,-823,-458\n\
    \-485,-357,347\n\
    \-345,-311,381\n\
    \-661,-816,-575\n\
    \-876,649,763\n\
    \-618,-824,-621\n\
    \553,345,-567\n\
    \474,580,667\n\
    \-447,-329,318\n\
    \-584,868,-557\n\
    \544,-627,-890\n\
    \564,392,-477\n\
    \455,729,728\n\
    \-892,524,684\n\
    \-689,845,-530\n\
    \423,-701,434\n\
    \7,-33,-71\n\
    \630,319,-379\n\
    \443,580,662\n\
    \-789,900,-551\n\
    \459,-707,401\n\
    \\n\
    \--- scanner 1 ---\n\
    \686,422,578\n\
    \605,423,415\n\
    \515,917,-361\n\
    \-336,658,858\n\
    \95,138,22\n\
    \-476,619,847\n\
    \-340,-569,-846\n\
    \567,-361,727\n\
    \-460,603,-452\n\
    \669,-402,600\n\
    \729,430,532\n\
    \-500,-761,534\n\
    \-322,571,750\n\
    \-466,-666,-811\n\
    \-429,-592,574\n\
    \-355,545,-477\n\
    \703,-491,-529\n\
    \-328,-685,520\n\
    \413,935,-424\n\
    \-391,539,-444\n\
    \586,-435,557\n\
    \-364,-763,-893\n\
    \807,-499,-711\n\
    \755,-354,-619\n\
    \553,889,-390\n\
    \\n\
    \--- scanner 2 ---\n\
    \649,640,665\n\
    \682,-795,504\n\
    \-784,533,-524\n\
    \-644,584,-595\n\
    \-588,-843,648\n\
    \-30,6,44\n\
    \-674,560,763\n\
    \500,723,-460\n\
    \609,671,-379\n\
    \-555,-800,653\n\
    \-675,-892,-343\n\
    \697,-426,-610\n\
    \578,704,681\n\
    \493,664,-388\n\
    \-671,-858,530\n\
    \-667,343,800\n\
    \571,-461,-707\n\
    \-138,-166,112\n\
    \-889,563,-600\n\
    \646,-828,498\n\
    \640,759,510\n\
    \-630,509,768\n\
    \-681,-892,-333\n\
    \673,-379,-804\n\
    \-742,-814,-386\n\
    \577,-820,562\n\
    \\n\
    \--- scanner 3 ---\n\
    \-589,542,597\n\
    \605,-692,669\n\
    \-500,565,-823\n\
    \-660,373,557\n\
    \-458,-679,-417\n\
    \-488,449,543\n\
    \-626,468,-788\n\
    \338,-750,-386\n\
    \528,-832,-391\n\
    \562,-778,733\n\
    \-938,-730,414\n\
    \543,643,-506\n\
    \-524,371,-870\n\
    \407,773,750\n\
    \-104,29,83\n\
    \378,-903,-323\n\
    \-778,-728,485\n\
    \426,699,580\n\
    \-438,-605,-362\n\
    \-469,-447,-387\n\
    \509,732,623\n\
    \647,635,-688\n\
    \-868,-804,481\n\
    \614,-800,639\n\
    \595,780,-596\n\
    \\n\
    \--- scanner 4 ---\n\
    \727,592,562\n\
    \-293,-554,779\n\
    \441,611,-461\n\
    \-714,465,-776\n\
    \-743,427,-804\n\
    \-660,-479,-426\n\
    \832,-632,460\n\
    \927,-485,-438\n\
    \408,393,-506\n\
    \466,436,-512\n\
    \110,16,151\n\
    \-258,-428,682\n\
    \-393,719,612\n\
    \-211,-452,876\n\
    \808,-476,-593\n\
    \-575,615,604\n\
    \-485,667,467\n\
    \-680,325,-822\n\
    \-627,-443,-432\n\
    \872,-547,-609\n\
    \833,512,582\n\
    \807,604,487\n\
    \839,-516,451\n\
    \891,-625,532\n\
    \-652,-548,-490\n\
    \30,-46,-14"

input :: IO String
input = readFile "input/2021/19December.txt"

december19Solution1 :: IO Int
december19Solution1 = (length . bacons . (\xs -> mergeAllScanners (head xs) (tail xs))) . parseInput <$> input

december19Solution2 :: IO Int
december19Solution2 = solution2 . parseInput <$> input
