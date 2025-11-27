module TwentyTwentyOne.December24 where

import Data.List (find, groupBy, maximumBy, nub, sortBy, sortOn, (\\))
import Data.Map (Map)
import qualified Data.Map as M (elems, empty, insert)
import Data.Maybe (catMaybes, fromJust, listToMaybe, mapMaybe)
import Data.Tree (Tree)
import qualified Data.Tree as T (foldTree, unfoldForest)
import Debug.Trace
import Text.Read

data State = State
    { reg_w :: Int
    , reg_x :: Int
    , reg_y :: Int
    , reg_z :: Int
    }
    deriving (Show)

data Instruction
    = InputInstruction String
    | Instruction String String (Either String Int)
    deriving (Show)

terminalState = State{reg_w = 0, reg_x = 0, reg_y = 0, reg_z = 0}

isInputInstruction :: Instruction -> Bool
isInputInstruction (InputInstruction _) = True
isInputInstruction _ = False

parseInput :: String -> [[Instruction]]
parseInput = filter (not . null) . splitAtInput . fmap parseInstruction . lines
  where
    parseInstruction =
        ( \s -> case head s of
            "inp" -> InputInstruction (s !! 1)
            _ -> Instruction (head s) (s !! 1) $ foldl (\_ v -> Right v) (Left (s !! 2)) (readMaybe (s !! 2) :: Maybe Int)
        )
            . words
    splitAtInput [] = []
    splitAtInput is = ((\(b, rest) -> b : splitAtInput (if null rest then [] else tail rest)) . break isInputInstruction) is

getValue :: State -> Either String Int -> Int
getValue State{reg_w = rw} (Left "w") = rw
getValue State{reg_x = rx} (Left "x") = rx
getValue State{reg_y = ry} (Left "y") = ry
getValue State{reg_z = rz} (Left "z") = rz
getValue _ (Right v) = v

alu :: State -> [Instruction] -> State
alu s [] = s
alu s ((Instruction "add" r v) : is) = alu (applyInstruction s (+) r v) is
alu s ((Instruction "mul" r v) : is) = alu (applyInstruction s (*) r v) is
alu s ((Instruction "div" r v) : is) = alu (applyInstruction s div r v) is
alu s ((Instruction "mod" r v) : is) = alu (applyInstruction s mod r v) is
alu s ((Instruction "eql" r v) : is) = alu (applyInstruction s (\x x' -> if x == x' then 1 else 0) r v) is
alu _ (i : _) = error $ "Unrecognized instruction: " ++ show i

applyInstruction :: State -> (Int -> Int -> Int) -> String -> Either String Int -> State
applyInstruction s@State{reg_w = rw, reg_x = rx, reg_y = ry, reg_z = rz} f r ev =
    case r of
        "w" -> s{reg_w = f rw (getValue s ev)}
        "x" -> s{reg_x = f rx (getValue s ev)}
        "y" -> s{reg_y = f ry (getValue s ev)}
        "z" -> s{reg_z = f rz (getValue s ev)}
        _ -> error $ "Unrecognized register: " ++ r

-- Tree having (w, z)
modelNumberTree :: Int -> [[Instruction]] -> [Tree (Int, Int)]
modelNumberTree zLimit bs =
    T.unfoldForest
        ( \(d, w, z) ->
            let b = bs !! d
                st = alu (State{reg_w = w, reg_x = 0, reg_y = 0, reg_z = z}) b
                z' = reg_z st
             in ((w, reg_z st), [(d + 1, w', z') | (d + 1) < length bs, z' < zLimit, w' <- [9, 8 .. 1]])
        )
        [(0, w, 0) | w <- [9, 8 .. 1]]

modelNumbers :: Tree (Int, Int) -> Maybe String
modelNumbers = T.foldTree treeToNum
  where
    treeToNum (w, z) [] = if z == 0 then Just (traceShow z (show w)) else Nothing
    treeToNum (w, _) xs =
        let vals = (fmap (\s -> read s :: Int) . catMaybes) xs
         in if null vals then Nothing else (Just . traceShowId . (show w ++) . show) $ maximum vals

search :: [[Instruction]] -> Maybe String
search = listToMaybe . mapMaybe modelNumbers . modelNumberTree 205000 . take 8

-- Not working, this es is crap, copying a java solution with a guy analyzing the input for me 🖕
test = search <$> input

input :: IO [[Instruction]]
input = parseInput <$> readFile "input/2021/24December.txt"

relevantInputValues :: Int -> [[Instruction]] -> [(Int, Int, Int)]
relevantInputValues _ [] = []
relevantInputValues i (b : bs) = (i, unsafeGetValue (b !! 4), unsafeGetValue (b !! 14)) : relevantInputValues (i + 1) bs
  where
    unsafeGetValue (Instruction _ _ (Right v)) = v

sillySolution1 :: [(Int, Int, Int)] -> Map Int Int -> [(Int, Int, Int)] -> Map Int Int
sillySolution1 _ monad [] = monad
sillySolution1 stack monad ((i, x, y) : bs)
    | x >= 10 = sillySolution1 (stack ++ [(i, x, y)]) monad bs
    | otherwise =
        let (li, _, ly) = last stack
         in if ly + x >= 0
                then sillySolution1 (init stack) (M.insert li (9 - (ly + x)) (M.insert i 9 monad)) bs
                else sillySolution1 (init stack) (M.insert i (9 + (ly + x)) (M.insert li 9 monad)) bs

twentyFourthDecemberSolution1 :: IO Int
twentyFourthDecemberSolution1 = do
    i <- input
    let rv = relevantInputValues 0 i
        ms = sillySolution1 [] M.empty rv
        s = ((\x -> read x :: Int) . concatMap show . M.elems) ms
    return s

sillySolution2 :: [(Int, Int, Int)] -> Map Int Int -> [(Int, Int, Int)] -> Map Int Int
sillySolution2 _ monad [] = monad
sillySolution2 stack monad ((i, x, y) : bs)
    | x >= 10 = sillySolution2 (stack ++ [(i, x, y)]) monad bs
    | otherwise =
        let (li, _, ly) = last stack
         in if ly + x >= 0
                then sillySolution2 (init stack) (M.insert li 1 (M.insert i (1 + (ly + x)) monad)) bs
                else sillySolution2 (init stack) (M.insert i 1 (M.insert li (1 - (ly + x)) monad)) bs

twentyFourthDecemberSolution2 :: IO Int
twentyFourthDecemberSolution2 = do
    i <- input
    let rv = relevantInputValues 0 i
        ms = sillySolution2 [] M.empty rv
        s = ((\x -> read x :: Int) . concatMap show . M.elems) ms
    return s
