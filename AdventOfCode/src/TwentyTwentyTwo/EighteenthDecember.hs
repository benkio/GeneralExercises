module TwentyTwentyTwo.EighteenthDecember where

import Data.List (group, partition, sort, (\\))

type Obsidian = (Int, Int, Int)
data ObsidianFace = ObsidianFace
    { vertex1 :: (Float, Float, Float)
    , vertex2 :: (Float, Float, Float)
    , vertex3 :: (Float, Float, Float)
    , vertex4 :: (Float, Float, Float)
    }
    deriving (Show, Ord)
type ObsidianSurface = [ObsidianFace]

instance Eq ObsidianFace where
    (==) o o' = null $ vertexes o \\ vertexes o'

vertexes :: ObsidianFace -> [(Float, Float, Float)]
vertexes o = [vertex1 o, vertex2 o, vertex3 o, vertex4 o]

parseInput = fmap parseObsidian . lines
  where
    parseX = takeWhile (/= ',')
    parseY = parseX . tail . dropWhile (/= ',')
    parseZ = parseY . tail . dropWhile (/= ',')
    parseObsidian s =
        ( read (parseX s) :: Int
        , read (parseY s) :: Int
        , read (parseZ s) :: Int
        )

input :: IO [Obsidian]
input = parseInput <$> readFile "input/2022/18December.txt"

testInput :: [Obsidian]
testInput =
    parseInput
        "2,2,2\n\
        \1,2,2\n\
        \3,2,2\n\
        \2,1,2\n\
        \2,3,2\n\
        \2,2,1\n\
        \2,2,3\n\
        \2,2,4\n\
        \2,2,6\n\
        \1,2,5\n\
        \3,2,5\n\
        \2,1,5\n\
        \2,3,5"

computeFaces :: Obsidian -> [ObsidianFace]
computeFaces (a, b, c) =
    [ ObsidianFace{vertex1 = (x + 0.5, y + 0.5, z + 0.5), vertex2 = (x + 0.5, y + 0.5, z - 0.5), vertex3 = (x + 0.5, y - 0.5, z + 0.5), vertex4 = (x + 0.5, y - 0.5, z - 0.5)}
    , ObsidianFace{vertex1 = (x - 0.5, y + 0.5, z + 0.5), vertex2 = (x - 0.5, y + 0.5, z - 0.5), vertex3 = (x - 0.5, y - 0.5, z + 0.5), vertex4 = (x - 0.5, y - 0.5, z - 0.5)}
    , ObsidianFace{vertex1 = (x + 0.5, y + 0.5, z + 0.5), vertex2 = (x + 0.5, y + 0.5, z - 0.5), vertex3 = (x - 0.5, y + 0.5, z + 0.5), vertex4 = (x - 0.5, y + 0.5, z - 0.5)}
    , ObsidianFace{vertex1 = (x + 0.5, y - 0.5, z + 0.5), vertex2 = (x + 0.5, y - 0.5, z - 0.5), vertex3 = (x - 0.5, y - 0.5, z + 0.5), vertex4 = (x - 0.5, y - 0.5, z - 0.5)}
    , ObsidianFace{vertex1 = (x + 0.5, y + 0.5, z + 0.5), vertex2 = (x + 0.5, y - 0.5, z + 0.5), vertex3 = (x - 0.5, y + 0.5, z + 0.5), vertex4 = (x - 0.5, y - 0.5, z + 0.5)}
    , ObsidianFace{vertex1 = (x + 0.5, y + 0.5, z - 0.5), vertex2 = (x + 0.5, y - 0.5, z - 0.5), vertex3 = (x - 0.5, y + 0.5, z - 0.5), vertex4 = (x - 0.5, y - 0.5, z - 0.5)}
    ]
  where
    x = fromIntegral a :: Float
    y = fromIntegral b :: Float
    z = fromIntegral c :: Float

freeFaces :: [Obsidian] -> [ObsidianFace]
freeFaces = concat . filter ((== 1) . length) . group . sort . concatMap computeFaces

solution1 :: [Obsidian] -> Int
solution1 = length . freeFaces

eighteenthDecemberSolution1 :: IO Int
eighteenthDecemberSolution1 = solution1 <$> input

connectedFaces :: ObsidianFace -> ObsidianFace -> Bool
connectedFaces o o' = ((== 2) . length) $ vertexes o \\ vertexes o'

expandSurface :: ObsidianSurface -> ObsidianSurface -> (ObsidianSurface, ObsidianSurface)
expandSurface s rest
    | null connected = (s, rest)
    | otherwise = expandSurface (s ++ connected) rest'
  where
    (connected, rest') = partition (\o ->
                                      -- TODO: can't match in inner intersections. Maybe limit this to 1 or 2 matches? 
                                      -- any (\o' -> connectedFaces o o') s 
                                   ) rest

findSurface :: ObsidianSurface -> [ObsidianSurface]
findSurface [] = []
findSurface (s:ss) = surface : findSurface rest
  where (surface, rest) = expandSurface [s] ss

eighteenthDecemberSolution2 :: IO Int
eighteenthDecemberSolution2 = undefined
