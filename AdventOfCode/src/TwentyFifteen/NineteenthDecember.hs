{-# LANGUAGE TupleSections #-}

module TwentyFifteen.NineteenthDecember where

import Data.Bifunctor
import Data.List
import Data.Map (Map, (!))
import Data.Map as Map (elemAt, foldMapWithKey, fromList, keys, toList)
import Data.Maybe
import Data.Ord
import System.Random

remove :: String -> String -> String -> (String, String)
remove acc _ "" = (acc, "")
remove acc w s@(c : cs)
    | w `isPrefixOf` s = (acc, drop (length w) s)
    | otherwise = remove (acc ++ [c]) w cs

type SubstitutionMap = Map String [String]

input :: IO (SubstitutionMap, String)
input =
    uncurry (parseInput []) . span ("" /=) . lines
        <$> readFile "input/2015/19December.txt"

parseInput ::
    [(String, String)] -> [String] -> [String] -> (SubstitutionMap, String)
parseInput m [] y =
    ( ( Map.fromList
            . fmap
                (foldl1 (\(x, v) (_, v') -> (x, v ++ v')) . fmap (\(x, a) -> (x, [a])))
            . groupBy (\(x, _) (x', _) -> x == x')
            . sortOn fst
      )
        m
    , last y
    )
parseInput m (x : xs) y = parseInput (remove "" " => " x : m) xs y

inputTest :: (SubstitutionMap, String)
inputTest =
    (uncurry (parseInput []) . span ("" /=) . lines)
        "H => HO\n\
        \H => OH\n\
        \O => HH\n\
        \\n\
        \HOHOHO"

inputTest2 :: (SubstitutionMap, String)
inputTest2 =
    (uncurry (parseInput []) . span ("" /=) . lines)
        "e => H\n\
        \e => O\n\
        \H => HO\n\
        \H => OH\n\
        \O => HH\n\
        \\n\
        \HOHOHO"

solution1 :: SubstitutionMap -> String -> [String]
solution1 m s = (nub . Map.foldMapWithKey (substituteMolecule s)) m

substituteMolecule :: String -> String -> [String] -> [String]
substituteMolecule "" _ _ = []
substituteMolecule x k subs =
    let (base, after) = remove "" k x
        newMolecules =
            if base ++ after /= x
                then fmap (\s -> base ++ s ++ after) subs
                else []
     in newMolecules
            ++ fmap (\a -> base ++ k ++ a) (substituteMolecule after k subs)

solution1Test :: Bool
solution1Test = length (uncurry solution1 inputTest) == 7

nineteenthDecemberSolution1 :: IO Int
nineteenthDecemberSolution1 = length . uncurry solution1 <$> input

solution2 :: SubstitutionMap -> String -> Int -> IO Int
solution2 m target step = do
    _ <- putStrLn ("Molecule: " ++ target)
    randomMoleculeIndex <- randomRIO (0, length (Map.keys m) - 1)
    let (moleculeK, moleculeV) = Map.elemAt randomMoleculeIndex m
        nextStep = substituteMolecule target moleculeK moleculeV
        (nextTarget, step') =
            if null nextStep
                then (target, step)
                else (head nextStep, step + 1)
    if "e" == nextTarget
        then return step'
        else solution2 m nextTarget step'

invertMap :: SubstitutionMap -> SubstitutionMap
invertMap = Map.fromList . concatMap (\(k, vs) -> fmap (,[k]) vs) . Map.toList

nineteenthDecemberSolution2 :: IO Int
nineteenthDecemberSolution2 =
    input >>= (\(m, target) -> solution2 (invertMap m) target 0)
