module TwentyFifteen.SixthDecember where

import qualified Data.Char as Char
import Data.Map as M (
    Map,
    adjust,
    alter,
    delete,
    elems,
    empty,
    insertWith,
    size,
 )
import Data.Maybe

type Coordinate = (Int, Int)

data LightStatus
    = On
    | Off
    | Toggle
    deriving (Show, Read, Eq)

data Instruction
    = Instruction LightStatus Coordinate Coordinate
    deriving (Show)

litLights :: M.Map Coordinate a
litLights = M.empty

capitalized :: String -> String
capitalized (h : t) = Char.toUpper h : map Char.toLower t
capitalized [] = []

input :: IO [Instruction]
input = fmap parseInstruction . lines <$> readFile "input/2015/6December.txt"

parseInstruction :: String -> Instruction
parseInstruction s =
    let ws = words s
        c2 = read ('(' : last ws ++ ")") :: (Int, Int)
        c1 = read ('(' : (ws !! (length ws - 3)) ++ ")") :: (Int, Int)
        t =
            if head ws == "turn"
                then ws !! 1
                else head ws
        t' = read (capitalized t) :: LightStatus
     in Instruction t' c1 c2

instructionToGrid :: Instruction -> [Coordinate]
instructionToGrid (Instruction _ (x1, y1) (x2, y2)) =
    [(x, y) | x <- [x1 .. x2], y <- [y1 .. y2]]

applyInstruction ::
    M.Map Coordinate LightStatus -> Instruction -> M.Map Coordinate LightStatus
applyInstruction lights i@(Instruction On _ _) =
    foldl
        (\m (x, y) -> M.insertWith (\_ _ -> On) (x, y) On m)
        lights
        (instructionToGrid i)
applyInstruction lights i@(Instruction Off _ _) =
    foldl (\m (x, y) -> M.delete (x, y) m) lights (instructionToGrid i)
applyInstruction lights i@(Instruction Toggle _ _) =
    foldl
        ( \m (x, y) ->
            M.alter
                ( \a ->
                    if isJust a
                        then Nothing
                        else Just On
                )
                (x, y)
                m
        )
        lights
        (instructionToGrid i)

solution1 :: [Instruction] -> Int
solution1 is = M.size $ foldl applyInstruction litLights is

adjustBrightness :: M.Map Coordinate Int -> Instruction -> M.Map Coordinate Int
adjustBrightness lights i@(Instruction On _ _) =
    foldl (\m (x, y) -> M.insertWith (+) (x, y) 1 m) lights (instructionToGrid i)
adjustBrightness lights i@(Instruction Off _ _) =
    foldl
        (\m (x, y) -> M.adjust (\a -> max 0 (a - 1)) (x, y) m)
        lights
        (instructionToGrid i)
adjustBrightness lights i@(Instruction Toggle _ _) =
    foldl (\m (x, y) -> M.insertWith (+) (x, y) 2 m) lights (instructionToGrid i)

solution2 :: [Instruction] -> Int
solution2 is = (sum . M.elems) $ foldl adjustBrightness litLights is

sixthDecemberSolution1 :: IO Int
sixthDecemberSolution1 = solution1 <$> input

sixthDecemberSolution2 :: IO Int
sixthDecemberSolution2 = solution2 <$> input
