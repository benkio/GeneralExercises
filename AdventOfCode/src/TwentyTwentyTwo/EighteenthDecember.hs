module TwentyTwentyTwo.EighteenthDecember where

import Data.List (find, groupBy, sortBy, (\\))
import Data.Maybe (catMaybes, listToMaybe)
import Debug.Trace

type Obsidian = (Int, Int, Int)
data Orientation = L | R | N | S | B | F deriving (Eq, Ord, Show)
data ObsidianFace = ObsidianFace
    { orientation :: Orientation
    , center :: (Float, Float, Float)
    }
    deriving (Eq, Ord, Show)

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
    [ ObsidianFace{orientation = R, center = (x + 0.5, y, z)}
    , ObsidianFace{orientation = L, center = (x - 0.5, y, z)}
    , ObsidianFace{orientation = N, center = (x, y + 0.5, z)}
    , ObsidianFace{orientation = S, center = (x, y - 0.5, z)}
    , ObsidianFace{orientation = F, center = (x, y, z + 0.5)}
    , ObsidianFace{orientation = B, center = (x, y, z - 0.5)}
    ]
  where
    x = fromIntegral a :: Float
    y = fromIntegral b :: Float
    z = fromIntegral c :: Float

freeFaces :: [Obsidian] -> [ObsidianFace]
freeFaces = concat . filter ((== 1) . length) . groupBy (\o o' -> center o == center o') . sortBy (\o o' -> center o `compare` center o') . concatMap computeFaces

solution1 :: [Obsidian] -> Int
solution1 = length . freeFaces

eighteenthDecemberSolution1 :: IO Int
eighteenthDecemberSolution1 = solution1 <$> input

-- 2440 too low
-- eighteenthDecemberSolution2 :: IO Int
eighteenthDecemberSolution2 = solution2 <$> input

connectFace :: ObsidianFace -> [ObsidianFace] -> [ObsidianFace] -> ([ObsidianFace], [ObsidianFace])
connectFace f ss fs = (connections, fs \\ connections)
  where
    possibleConnections = connectionFaces f
    connections = ((\\ ss) . catMaybes) $ find (\x -> x `elem` (fs ++ ss)) <$> possibleConnections

expandSurface :: [ObsidianFace] -> [ObsidianFace] -> [ObsidianFace] -> ([ObsidianFace], [ObsidianFace])
expandSurface expanded surface fs = maybe (surface, fs) loop mayS
  where
    mayS = find (`notElem` expanded) surface
    loop s =
        let (connections, fs') = connectFace s surface fs
            expanded' = s : expanded
            surface' = (connections ++ surface)
         in expandSurface expanded' surface' fs'

findSurface :: [ObsidianFace] -> ([ObsidianFace], [ObsidianFace])
findSurface fs = expandSurface [] [head fs] (tail fs)

solution2 = length . fst . findSurface . freeFaces

-- Given an obsidian face it generates the allowed connectionFaces
-- requirement: Consider the Right
--   a face on the right can't connect with a face on the left and so on
--   a face on the right can connect with an offest of 0.5 on x and on another dimention
--   a face on the right can connect with an offset of 0 on x and 1 on another dimension
--   ordering is important: prefer first the 0.5 offset, then the 1 case, then the -0,5 case
connectionFaces :: ObsidianFace -> [[ObsidianFace]]
connectionFaces (ObsidianFace{orientation = R, center = (x, y, z)}) =
    [ -- North Connection

        [ ObsidianFace{orientation = S, center = (x + 0.5, y + 0.5, z)}
        , ObsidianFace{orientation = R, center = (x, y + 1, z)}
        , ObsidianFace{orientation = N, center = (x - 0.5, y + 0.5, z)}
        ]
    , -- Back Connection

        [ ObsidianFace{orientation = F, center = (x + 0.5, y, z - 0.5)}
        , ObsidianFace{orientation = R, center = (x, y, z - 1)}
        , ObsidianFace{orientation = B, center = (x - 0.5, y, z - 0.5)}
        ]
    , -- South Connection

        [ ObsidianFace{orientation = N, center = (x + 0.5, y - 0.5, z)}
        , ObsidianFace{orientation = R, center = (x, y - 1, z)}
        , ObsidianFace{orientation = S, center = (x - 0.5, y - 0.5, z)}
        ]
    , -- Front Connection

        [ ObsidianFace{orientation = B, center = (x + 0.5, y, z + 0.5)}
        , ObsidianFace{orientation = R, center = (x, y, z + 1)}
        , ObsidianFace{orientation = F, center = (x - 0.5, y, z + 0.5)}
        ]
    ]
connectionFaces (ObsidianFace{orientation = L, center = (x, y, z)}) =
    [ -- North Connection

        [ ObsidianFace{orientation = S, center = (x - 0.5, y + 0.5, z)}
        , ObsidianFace{orientation = L, center = (x, y + 1, z)}
        , ObsidianFace{orientation = N, center = (x + 0.5, y + 0.5, z)}
        ]
    , -- Back Connection

        [ ObsidianFace{orientation = F, center = (x - 0.5, y, z - 0.5)}
        , ObsidianFace{orientation = L, center = (x, y, z - 1)}
        , ObsidianFace{orientation = B, center = (x + 0.5, y, z - 0.5)}
        ]
    , -- South Connection

        [ ObsidianFace{orientation = N, center = (x - 0.5, y - 0.5, z)}
        , ObsidianFace{orientation = L, center = (x, y - 1, z)}
        , ObsidianFace{orientation = S, center = (x + 0.5, y - 0.5, z)}
        ]
    , -- Front Connection

        [ ObsidianFace{orientation = B, center = (x - 0.5, y, z + 0.5)}
        , ObsidianFace{orientation = L, center = (x, y, z + 1)}
        , ObsidianFace{orientation = F, center = (x + 0.5, y, z + 0.5)}
        ]
    ]
connectionFaces (ObsidianFace{orientation = N, center = (x, y, z)}) =
    [ -- Right Connection

        [ ObsidianFace{orientation = L, center = (x + 0.5, y + 0.5, z)}
        , ObsidianFace{orientation = N, center = (x + 1, y, z)}
        , ObsidianFace{orientation = R, center = (x + 0.5, y - 0.5, z)}
        ]
    , -- Back Connection

        [ ObsidianFace{orientation = F, center = (x, y + 0.5, z - 0.5)}
        , ObsidianFace{orientation = N, center = (x, y, z - 1)}
        , ObsidianFace{orientation = B, center = (x, y - 0.5, z - 0.5)}
        ]
    , -- Left Connection

        [ ObsidianFace{orientation = R, center = (x - 0.5, y + 0.5, z)}
        , ObsidianFace{orientation = N, center = (x - 1, y, z)}
        , ObsidianFace{orientation = L, center = (x - 0.5, y - 0.5, z)}
        ]
    , -- Front Connection

        [ ObsidianFace{orientation = B, center = (x, y + 0.5, z + 0.5)}
        , ObsidianFace{orientation = N, center = (x, y, z + 1)}
        , ObsidianFace{orientation = F, center = (x, y - 0.5, z + 0.5)}
        ]
    ]
connectionFaces (ObsidianFace{orientation = S, center = (x, y, z)}) =
    [ -- Right Connection

        [ ObsidianFace{orientation = L, center = (x + 0.5, y - 0.5, z)}
        , ObsidianFace{orientation = S, center = (x + 1, y, z)}
        , ObsidianFace{orientation = R, center = (x + 0.5, y + 0.5, z)}
        ]
    , -- Back Connection

        [ ObsidianFace{orientation = F, center = (x, y - 0.5, z - 0.5)}
        , ObsidianFace{orientation = S, center = (x, y, z - 1)}
        , ObsidianFace{orientation = B, center = (x, y + 0.5, z - 0.5)}
        ]
    , -- Left Connection

        [ ObsidianFace{orientation = R, center = (x - 0.5, y - 0.5, z)}
        , ObsidianFace{orientation = S, center = (x - 1, y, z)}
        , ObsidianFace{orientation = L, center = (x - 0.5, y + 0.5, z)}
        ]
    , -- Front Connection

        [ ObsidianFace{orientation = B, center = (x, y - 0.5, z + 0.5)}
        , ObsidianFace{orientation = S, center = (x, y, z + 1)}
        , ObsidianFace{orientation = F, center = (x, y + 0.5, z + 0.5)}
        ]
    ]
connectionFaces (ObsidianFace{orientation = B, center = (x, y, z)}) =
    [ -- Right Connectionn

        [ ObsidianFace{orientation = L, center = (x + 0.5, y, z - 0.5)}
        , ObsidianFace{orientation = B, center = (x + 1, y, z)}
        , ObsidianFace{orientation = R, center = (x + 0.5, y, z + 0.5)}
        ]
    , -- North Connection

        [ ObsidianFace{orientation = S, center = (x, y + 0.5, z - 0.5)}
        , ObsidianFace{orientation = B, center = (x, y + 1, z)}
        , ObsidianFace{orientation = N, center = (x, y + 0.5, z + 0.5)}
        ]
    , -- Left Connection

        [ ObsidianFace{orientation = R, center = (x - 0.5, y, z - 0.5)}
        , ObsidianFace{orientation = B, center = (x - 1, y, z)}
        , ObsidianFace{orientation = L, center = (x - 0.5, y, z + 0.5)}
        ]
    , -- South Connection

        [ ObsidianFace{orientation = N, center = (x, y - 0.5, z - 0.5)}
        , ObsidianFace{orientation = B, center = (x, y - 1, z)}
        , ObsidianFace{orientation = S, center = (x, y - 0.5, z + 0.5)}
        ]
    ]
connectionFaces (ObsidianFace{orientation = F, center = (x, y, z)}) =
    [ -- Right Connectionn

        [ ObsidianFace{orientation = L, center = (x + 0.5, y, z + 0.5)}
        , ObsidianFace{orientation = F, center = (x + 1, y, z)}
        , ObsidianFace{orientation = R, center = (x + 0.5, y, z - 0.5)}
        ]
    , -- North Connection

        [ ObsidianFace{orientation = S, center = (x, y + 0.5, z + 0.5)}
        , ObsidianFace{orientation = F, center = (x, y + 1, z)}
        , ObsidianFace{orientation = N, center = (x, y + 0.5, z - 0.5)}
        ]
    , -- Left Connection

        [ ObsidianFace{orientation = R, center = (x - 0.5, y, z + 0.5)}
        , ObsidianFace{orientation = F, center = (x - 1, y, z)}
        , ObsidianFace{orientation = L, center = (x - 0.5, y, z - 0.5)}
        ]
    , -- South Connection

        [ ObsidianFace{orientation = N, center = (x, y - 0.5, z + 0.5)}
        , ObsidianFace{orientation = F, center = (x, y - 1, z)}
        , ObsidianFace{orientation = S, center = (x, y - 0.5, z - 0.5)}
        ]
    ]
