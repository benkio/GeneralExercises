module TwentyTwentyOne.TwentySecondDecember where

import Data.Bifunctor (bimap)
import Data.List ((\\))
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as S (empty, filter, foldl, fromList, insert, map, notMember, null, size, toList, union, unions)
import Data.Text (Text)
import qualified Data.Text as T (break, drop, pack, splitOn, unpack)
import Debug.Trace
import Text.Printf

data Cuboid = Cuboid
    { status :: Bool
    , maxX :: Int
    , minX :: Int
    , maxY :: Int
    , minY :: Int
    , maxZ :: Int
    , minZ :: Int
    }
    deriving (Eq, Ord, Show)

-- instance Show Cuboid where
--   show Cuboid {status = st, maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} =
--     printf "(%s x=%d..%d - y=%d..%d - z=%d..%d)" (show st) cMinX cMaxX cMinY cMaxY cMinZ cMaxZ

cubesInCuboid :: Cuboid -> Int
cubesInCuboid Cuboid{status = st, maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} =
    let v = (cMaxX - cMinX + 1) * (cMaxY - cMinY + 1) * (cMaxZ - cMinZ + 1) in if st then v else v * (-1)

isArea :: Cuboid -> Bool
isArea Cuboid{maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} =
    (cMaxX == cMinX) && (cMaxY /= cMinY) && (cMaxZ /= cMinZ)
        || (cMaxX /= cMinX) && (cMaxY == cMinY) && (cMaxZ /= cMinZ)
        || (cMaxX /= cMinX) && (cMaxY /= cMinY) && (cMaxZ == cMinZ)

isEdge :: Cuboid -> Bool
isEdge Cuboid{maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} =
    (cMaxX == cMinX) && (cMaxY == cMinY) && (cMaxZ /= cMinZ)
        || (cMaxX /= cMinX) && (cMaxY == cMinY) && (cMaxZ == cMinZ)
        || (cMaxX == cMinX) && (cMaxY /= cMinY) && (cMaxZ == cMinZ)

isPoint :: Cuboid -> Bool
isPoint Cuboid{maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} =
    (cMaxX == cMinX) && (cMaxY == cMinY) && (cMaxZ == cMinZ)

isCuboid :: Cuboid -> Bool
isCuboid c = not (isPoint c) && not (isEdge c) && not (isArea c)

isBig :: Cuboid -> Bool
isBig Cuboid{maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} =
    cMaxX > 50 || cMinX < (-50) || cMaxY > 50 || cMinY < (-50) || cMaxZ > 50 || cMinZ < (-50)

intersectCuboidsPlus1 :: Cuboid -> Cuboid -> Cuboid
intersectCuboidsPlus1 Cuboid{maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} Cuboid{maxX = cMaxX', minX = cMinX', maxY = cMaxY', minY = cMinY', maxZ = cMaxZ', minZ = cMinZ'} =
    Cuboid{status = False, maxX = min cMaxX cMaxX' + 1, minX = max cMinX cMinX' - 1, maxY = min cMaxY cMaxY' + 1, minY = max cMinY cMinY' - 1, maxZ = min cMaxZ cMaxZ' + 1, minZ = max cMinZ cMinZ' - 1}

isIntersection :: Cuboid -> Cuboid -> Bool
isIntersection Cuboid{maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} Cuboid{maxX = cMaxX', minX = cMinX', maxY = cMaxY', minY = cMinY', maxZ = cMaxZ', minZ = cMinZ'} =
    (((cMaxX >= cMinX' && cMaxX <= cMaxX') || (cMinX >= cMinX' && cMinX <= cMaxX')) || ((cMaxX' >= cMinX && cMaxX' <= cMaxX) || (cMinX' >= cMinX && cMinX' <= cMaxX)))
        && (((cMaxY >= cMinY' && cMaxY <= cMaxY') || (cMinY >= cMinY' && cMinY <= cMaxY')) || ((cMaxY' >= cMinY && cMaxY' <= cMaxY) || (cMinY' >= cMinY && cMinY' <= cMaxY)))
        && (((cMaxZ >= cMinZ' && cMaxZ <= cMaxZ') || (cMinZ >= cMinZ' && cMinZ <= cMaxZ')) || ((cMaxZ' >= cMinZ && cMaxZ' <= cMaxZ) || (cMinZ' >= cMinZ && cMinZ' <= cMaxZ)))

isContained :: Cuboid -> Cuboid -> Bool
isContained c@Cuboid{maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} c'@Cuboid{maxX = cMaxX', minX = cMinX', maxY = cMaxY', minY = cMinY', maxZ = cMaxZ', minZ = cMinZ'} =
    (cMaxX > cMinX' && cMaxX < cMaxX') && (cMinX > cMinX' && cMinX < cMaxX') && (cMaxY > cMinY' && cMaxY < cMaxY') && (cMinY > cMinY' && cMinY < cMaxY') && (cMaxZ > cMinZ' && cMaxZ < cMaxZ') && (cMinZ > cMinZ' && cMinZ < cMaxZ')

cropCuboid :: Cuboid -> Cuboid -> [Cuboid]
cropCuboid c@Cuboid{maxX = cMaxX, minX = cMinX, maxY = cMaxY, minY = cMinY, maxZ = cMaxZ, minZ = cMinZ} c'@Cuboid{maxX = cMaxX', minX = cMinX', maxY = cMaxY', minY = cMinY', maxZ = cMaxZ', minZ = cMinZ'} =
    let ci = intersectCuboidsPlus1 c c'
        cropMaxX = if cMaxX >= cMinX' && cMaxX <= cMaxX' then Just (c'{minX = maxX ci}) else Nothing
        cropMinX = if cMinX >= cMinX' && cMinX <= cMaxX' then Just (c'{maxX = minX ci}) else Nothing
        cropMaxY = if cMaxY >= cMinY' && cMaxY <= cMaxY' then Just (c'{minX = minX ci + 1, maxX = maxX ci - 1, minY = maxY ci}) else Nothing
        cropMinY = if cMinY >= cMinY' && cMinY <= cMaxY' then Just (c'{minX = minX ci + 1, maxX = maxX ci - 1, maxY = minY ci}) else Nothing
        cropMaxZ = if cMaxZ >= cMinZ' && cMaxZ <= cMaxZ' then Just (c'{minX = minX ci + 1, maxX = maxX ci - 1, maxY = maxY ci - 1, minY = minY ci + 1, minZ = maxZ ci}) else Nothing
        cropMinZ = if cMinZ >= cMinZ' && cMinZ <= cMaxZ' then Just (c'{minX = minX ci + 1, maxX = maxX ci - 1, maxY = maxY ci - 1, minY = minY ci + 1, maxZ = minZ ci}) else Nothing
     in catMaybes [cropMaxX, cropMinX, cropMaxY, cropMinY, cropMaxZ, cropMinZ]

-- Test use
cubesInCuboid' c = [(x, y, z) | x <- [minX c .. maxX c], y <- [minY c .. maxY c], z <- [minZ c .. maxZ c]]

bruteForceCubeCount :: [(Int, Int, Int)] -> [Cuboid] -> Int
bruteForceCubeCount cubes [] = length cubes
bruteForceCubeCount cubes (c : cs) = bruteForceCubeCount (cubes ++ cubesInCuboid' c) cs

addCuboid :: Cuboid -> [Cuboid] -> [Cuboid]
addCuboid c' [] = [c' | status c']
addCuboid c' (c : cs)
    | c `isContained` c' = addCuboid c' cs
    | (c' `isContained` c || c' `isIntersection` c) && status c = cropCuboid c' c ++ addCuboid c' cs -- ++ cs ++ [c' | status c']
    | otherwise = c : addCuboid c' cs

solution :: (Cuboid -> Bool) -> String -> Int
solution f = foldl (\acc x -> acc + cubesInCuboid x) 0 . foldl (flip addCuboid) [] . filter f . parseInput

solution1 :: String -> Int
solution1 = solution (not . isBig)

solution2 :: String -> Int
solution2 = solution (const True)

input :: IO String
input = readFile "input/2021/22December.txt"

twentySecondDecemberSolution1 :: IO Int
twentySecondDecemberSolution1 = solution1 <$> input

twentySecondDecemberSolution2 :: IO Int
twentySecondDecemberSolution2 = solution2 <$> input

parseInput :: String -> [Cuboid]
parseInput =
    fmap
        ( ( \(status, coordinates) ->
                Cuboid
                    { status = status == "on"
                    , maxX = (snd . (!! 0)) $ parseCoordinates (tail coordinates)
                    , minX = (fst . (!! 0)) $ parseCoordinates (tail coordinates)
                    , maxY = (snd . (!! 1)) $ parseCoordinates (tail coordinates)
                    , minY = (fst . (!! 1)) $ parseCoordinates (tail coordinates)
                    , maxZ = (snd . (!! 2)) $ parseCoordinates (tail coordinates)
                    , minZ = (fst . (!! 2)) $ parseCoordinates (tail coordinates)
                    }
          )
            . break (== ' ')
        )
        . lines
  where
    parseCoordinates :: String -> [(Int, Int)]
    parseCoordinates = fmap parseCoordinate . T.splitOn (T.pack ",") . T.pack
    parseCoordinate :: Text -> (Int, Int)
    parseCoordinate = bimap ((\x -> read x :: Int) . T.unpack) ((\x -> read x :: Int) . T.unpack . T.drop 2) . T.break (== '.') . T.drop 2

inputTest' :: String
inputTest' =
    "on x=10..12,y=10..12,z=10..12\n\
    \on x=11..13,y=11..13,z=11..13\n\
    \off x=9..11,y=9..11,z=9..11\n\
    \on x=10..10,y=10..10,z=10..10"

inputTest :: String
inputTest =
    "on x=-20..26,y=-36..17,z=-47..7\n\
    \on x=-20..33,y=-21..23,z=-26..28\n\
    \on x=-22..28,y=-29..23,z=-38..16\n\
    \on x=-46..7,y=-6..46,z=-50..-1\n\
    \on x=-49..1,y=-3..46,z=-24..28\n\
    \on x=2..47,y=-22..22,z=-23..27\n\
    \on x=-27..23,y=-28..26,z=-21..29\n\
    \on x=-39..5,y=-6..47,z=-3..44\n\
    \on x=-30..21,y=-8..43,z=-13..34\n\
    \on x=-22..26,y=-27..20,z=-29..19\n\
    \off x=-48..-32,y=26..41,z=-47..-37\n\
    \on x=-12..35,y=6..50,z=-50..-2\n\
    \off x=-48..-32,y=-32..-16,z=-15..-5\n\
    \on x=-18..26,y=-33..15,z=-7..46\n\
    \off x=-40..-22,y=-38..-28,z=23..41\n\
    \on x=-16..35,y=-41..10,z=-47..6\n\
    \off x=-32..-23,y=11..30,z=-14..3\n\
    \on x=-49..-5,y=-3..45,z=-29..18\n\
    \off x=18..30,y=-20..-8,z=-3..13\n\
    \on x=-41..9,y=-7..43,z=-33..15\n\
    \on x=-54112..-39298,y=-85059..-49293,z=-27449..7877\n\
    \on x=967..23432,y=45373..81175,z=27513..53682"

inputTest'' :: String
inputTest'' =
    "on x=-5..47,y=-31..22,z=-19..33\n\
    \on x=-44..5,y=-27..21,z=-14..35\n\
    \on x=-49..-1,y=-11..42,z=-10..38\n\
    \on x=-20..34,y=-40..6,z=-44..1\n\
    \off x=26..39,y=40..50,z=-2..11\n\
    \on x=-41..5,y=-41..6,z=-36..8\n\
    \off x=-43..-33,y=-45..-28,z=7..25\n\
    \on x=-33..15,y=-32..19,z=-34..11\n\
    \off x=35..47,y=-46..-34,z=-11..5\n\
    \on x=-14..36,y=-6..44,z=-16..29\n\
    \on x=-57795..-6158,y=29564..72030,z=20435..90618\n\
    \on x=36731..105352,y=-21140..28532,z=16094..90401\n\
    \on x=30999..107136,y=-53464..15513,z=8553..71215\n\
    \on x=13528..83982,y=-99403..-27377,z=-24141..23996\n\
    \on x=-72682..-12347,y=18159..111354,z=7391..80950\n\
    \on x=-1060..80757,y=-65301..-20884,z=-103788..-16709\n\
    \on x=-83015..-9461,y=-72160..-8347,z=-81239..-26856\n\
    \on x=-52752..22273,y=-49450..9096,z=54442..119054\n\
    \on x=-29982..40483,y=-108474..-28371,z=-24328..38471\n\
    \on x=-4958..62750,y=40422..118853,z=-7672..65583\n\
    \on x=55694..108686,y=-43367..46958,z=-26781..48729\n\
    \on x=-98497..-18186,y=-63569..3412,z=1232..88485\n\
    \on x=-726..56291,y=-62629..13224,z=18033..85226\n\
    \on x=-110886..-34664,y=-81338..-8658,z=8914..63723\n\
    \on x=-55829..24974,y=-16897..54165,z=-121762..-28058\n\
    \on x=-65152..-11147,y=22489..91432,z=-58782..1780\n\
    \on x=-120100..-32970,y=-46592..27473,z=-11695..61039\n\
    \on x=-18631..37533,y=-124565..-50804,z=-35667..28308\n\
    \on x=-57817..18248,y=49321..117703,z=5745..55881\n\
    \on x=14781..98692,y=-1341..70827,z=15753..70151\n\
    \on x=-34419..55919,y=-19626..40991,z=39015..114138\n\
    \on x=-60785..11593,y=-56135..2999,z=-95368..-26915\n\
    \on x=-32178..58085,y=17647..101866,z=-91405..-8878\n\
    \on x=-53655..12091,y=50097..105568,z=-75335..-4862\n\
    \on x=-111166..-40997,y=-71714..2688,z=5609..50954\n\
    \on x=-16602..70118,y=-98693..-44401,z=5197..76897\n\
    \on x=16383..101554,y=4615..83635,z=-44907..18747\n\
    \off x=-95822..-15171,y=-19987..48940,z=10804..104439\n\
    \on x=-89813..-14614,y=16069..88491,z=-3297..45228\n\
    \on x=41075..99376,y=-20427..49978,z=-52012..13762\n\
    \on x=-21330..50085,y=-17944..62733,z=-112280..-30197\n\
    \on x=-16478..35915,y=36008..118594,z=-7885..47086\n\
    \off x=-98156..-27851,y=-49952..43171,z=-99005..-8456\n\
    \off x=2032..69770,y=-71013..4824,z=7471..94418\n\
    \on x=43670..120875,y=-42068..12382,z=-24787..38892\n\
    \off x=37514..111226,y=-45862..25743,z=-16714..54663\n\
    \off x=25699..97951,y=-30668..59918,z=-15349..69697\n\
    \off x=-44271..17935,y=-9516..60759,z=49131..112598\n\
    \on x=-61695..-5813,y=40978..94975,z=8655..80240\n\
    \off x=-101086..-9439,y=-7088..67543,z=33935..83858\n\
    \off x=18020..114017,y=-48931..32606,z=21474..89843\n\
    \off x=-77139..10506,y=-89994..-18797,z=-80..59318\n\
    \off x=8476..79288,y=-75520..11602,z=-96624..-24783\n\
    \on x=-47488..-1262,y=24338..100707,z=16292..72967\n\
    \off x=-84341..13987,y=2429..92914,z=-90671..-1318\n\
    \off x=-37810..49457,y=-71013..-7894,z=-105357..-13188\n\
    \off x=-27365..46395,y=31009..98017,z=15428..76570\n\
    \off x=-70369..-16548,y=22648..78696,z=-1892..86821\n\
    \on x=-53470..21291,y=-120233..-33476,z=-44150..38147\n\
    \off x=-93533..-4276,y=-16170..68771,z=-104985..-24507"
