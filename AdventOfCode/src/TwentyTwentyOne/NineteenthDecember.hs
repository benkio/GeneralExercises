module TwentyTwentyOne.NineteenthDecember where

import Data.List (groupBy, transpose)
import Data.Map (Map)
import qualified Data.Map as M (filter, fromList, map, size, toList)
import Data.Maybe (fromJust, isJust)

data Scanner = Scanner Int [(Int, Int, Int)] deriving (Show)

bacons :: Scanner -> [(Int, Int, Int)]
bacons (Scanner _ bs) = bs

coordinateTransformation :: [(Int, Int, Int) -> (Int, Int, Int)]
coordinateTransformation =
  [ \(bx, by, bz) -> (bx, by, bz),
    \(bx, by, bz) -> (by, bz, bx),
    \(bx, by, bz) -> (bz, bx, by),
    \(bx, by, bz) -> (bx, by, - bz),
    \(bx, by, bz) -> (by, - bz, bx),
    \(bx, by, bz) -> (- bz, bx, by),
    \(bx, by, bz) -> (bx, - by, bz),
    \(bx, by, bz) -> (- by, bz, bx),
    \(bx, by, bz) -> (bz, bx, - by),
    \(bx, by, bz) -> (bx, - by, - bz),
    \(bx, by, bz) -> (- by, - bz, bx),
    \(bx, by, bz) -> (- bz, bx, - by),
    \(bx, by, bz) -> (- bx, by, bz),
    \(bx, by, bz) -> (by, bz, - bx),
    \(bx, by, bz) -> (bz, - bx, by),
    \(bx, by, bz) -> (- bx, by, - bz),
    \(bx, by, bz) -> (by, - bz, - bx),
    \(bx, by, bz) -> (- bz, - bx, by),
    \(bx, by, bz) -> (- bx, - by, bz),
    \(bx, by, bz) -> (- by, bz, - bx),
    \(bx, by, bz) -> (bz, - bx, - by),
    \(bx, by, bz) -> (- bx, - by, - bz),
    \(bx, by, bz) -> (- by, - bz, - bx),
    \(bx, by, bz) -> (- bz, - bx, - by)
  ]

changePerspective :: [(Int, Int, Int)] -> [[(Int, Int, Int)]]
changePerspective bs = transpose $ fmap (\(bx, by, bz) -> [v | x <- [bx, - bx], y <- [by, - by], z <- [bz, - bz], v <- [(x, y, z), (y, z, x), (z, x, y)]]) bs

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

parseCoordinate :: String -> (Int, Int, Int)
parseCoordinate s =
  let c1 = takeWhile (/= ',') s
      c2 = (takeWhile (/= ',') . tail . dropWhile (/= ',')) s
      c3 = (tail . dropWhile (/= ',') . tail . dropWhile (/= ',')) s
   in (read c1 :: Int, read c2 :: Int, read c3 :: Int)

baconPov :: [(Int, Int, Int)] -> Map (Int, Int, Int) [(Int, Int, Int)]
baconPov bs = M.fromList $ buildPov bs bs
  where
    buildPov [] _ = []
    buildPov (b@(bx, by, bz) : bs') bs = (b, filter (/= (0, 0, 0)) $ [(bx - bx', by - by', bz - bz') | (bx', by', bz') <- bs]) : buildPov bs' bs

matchBacon :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> Bool
matchBacon b b' = any (checkBaconPerspective 0 b) $ changePerspective b'

checkBaconPerspective :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)] -> Bool
checkBaconPerspective c [] _ = c >= 11
checkBaconPerspective c (y : ys) zs
  | c == 11 = True
  | y `elem` zs = checkBaconPerspective (c + 1) ys zs
  | otherwise = checkBaconPerspective c ys zs

matchScanner :: Scanner -> Scanner -> (Bool, [((Int, Int, Int), (Int, Int, Int))])
matchScanner (Scanner _ bs) (Scanner _ bs') =
  let bsr = baconPov bs
      bsr' = baconPov bs'
      searchMap = M.filter isJust $ M.map (searchInScanner (M.toList bsr')) bsr
   in (M.size searchMap >= 12, M.toList (M.map fromJust searchMap))

searchInScanner :: [((Int, Int, Int), [(Int, Int, Int)])] -> [(Int, Int, Int)] -> Maybe (Int, Int, Int)
searchInScanner [] sbs = Nothing
searchInScanner ((k, bs) : bs') sbs =
  if matchBacon sbs bs then Just k else searchInScanner bs' sbs

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
  \553,889,-390"

input :: IO String
input = readFile "input/2021/19December.txt"

nineteenthDecemberSolution1 :: IO Int
nineteenthDecemberSolution1 = undefined

nineteenthDecemberSolution2 :: IO Int
nineteenthDecemberSolution2 = undefined
