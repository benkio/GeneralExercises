{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.TwentiethDecember where

import Data.Bifunctor (first)

import Text.Printf (printf)

import Data.List (isPrefixOf)
import Data.Map (Map, adjust, empty, fromList, insert, toList)
import qualified Data.Map as Map (foldr, lookup)
import Data.Maybe (fromJust, isNothing)
import Debug.Trace

data Pulse = P Bool String String deriving (Show)
type Modules = Map String Module
data Module
    = FF String Bool [String]
    | C String (Map String Bool) [String]
    | BR [String]
    deriving (Show)

getModuleKey :: Module -> String
getModuleKey (FF k _ _) = k
getModuleKey (C k _ _) = k
getModuleKey (BR _) = "broadcaster"
getModuleOutput :: Module -> [String]
getModuleOutput (FF _ _ o) = o
getModuleOutput (C _ _ o) = o
getModuleOutput (BR o) = o
getModuleMem :: Module -> Map String Bool
getModuleMem (C _ m _) = m

addInput :: Module -> String -> Module
addInput (C s im outs) i = C s (insert i False im) outs
addInput m i = m

class ProcessPulse a where
    processPulse :: a -> Pulse -> (a, [Pulse])

instance ProcessPulse Module where
    processPulse m@(FF _ s ds) (P True _ _) = (m, [])
    processPulse (FF k s ds) p@(P False _ _) = (FF k (not s) ds, fmap (setPulseSrc k . setPulseValue (not s) . (`setPulseDst` p)) ds)
    processPulse (C s mem out) p@(P v src _) =
        let c' = C s (insert src v mem) out
            mem' = (toList . getModuleMem) c'
         in if all id (fmap snd mem')
                then (c', fmap (\k -> P False s k) out)
                else (c', fmap (\k -> P True s k) out)
    processPulse m@(BR ds) p = (m, (fmap (setPulseSrc "broadcaster" . (`setPulseDst` p)) ds))

setPulseValue :: Bool -> Pulse -> Pulse
setPulseValue b (P _ s d) = P b s d

setPulseSrc :: String -> Pulse -> Pulse
setPulseSrc src (P v _ d) = P v src d

setPulseDst :: String -> Pulse -> Pulse
setPulseDst dst (P v s _) = P v s dst
getPulseDst :: Pulse -> String
getPulseDst (P _ _ d) = d

input :: IO Modules
input = parseInput <$> readFile "input/2023/20December.txt"

parseInput :: String -> Modules
parseInput = updateConjunctionInputs . fromList . fmap ((\m -> (getModuleKey m, m)) . parseModule) . lines
  where
    parseModule s
        | head s == '%' = FF (parseModuleName s) False (parseModuleDestinationList s)
        | head s == '&' = C (parseModuleName s) empty (parseModuleDestinationList s)
        | "broadcaster" `isPrefixOf` s = BR $ parseModuleDestinationList s
    parseModuleName :: String -> String
    parseModuleName = takeWhile (/= ' ') . drop 1
    parseModuleDestinationList :: String -> [String]
    parseModuleDestinationList = fmap (\s -> if last s == ',' then init s else s) . words . drop 2 . dropWhile (/= '>')

updateConjunctionInputs :: Modules -> Modules
updateConjunctionInputs m = Map.foldr updateInputs m m
  where
    updateInputs :: Module -> Modules -> Modules
    updateInputs x m' =
        foldl (\m'' o -> adjust (`addInput` (getModuleKey x)) o m'') m' (getModuleOutput x)

pushButton :: Modules -> ((Int, Int), Bool, Modules)
pushButton m = go m (1, 0) False $ snd $ processPulse broacastModule (P False "aptly" "broadcaster")
  where
    broacastModule = fromJust $ Map.lookup "broadcaster" m
    updateCounter (lp, hp) (P True _ _) = (lp, hp + 1)
    updateCounter (lp, hp) (P False _ _) = (lp + 1, hp)
    activateRX (P False _ "rx") = True
    activateRX _ = False
    go :: Modules -> (Int, Int) -> Bool -> [Pulse] -> ((Int, Int), Bool, Modules)
    go m c rx [] = (c, rx, m)
    go m c rx (p : ps)
        | isNothing (Map.lookup (getPulseDst p) m) = go m (updateCounter c p) (rx || activateRX p) ps
        | otherwise =
            let
                pDst = getPulseDst p
                modul = fromJust $ Map.lookup pDst m
                (modul', pulses) = processPulse modul p
                m' = insert pDst modul' m
             in
                go m' (updateCounter c p) (rx || activateRX p) (ps ++ pulses)

solution1 m =
    (\(x, y) -> x * y)
        . (\(c, _, _) -> c)
        . (!! 1000)
        $ iterate (\((plp, php), _, x) -> (\((clp, chp), a, b) -> ((plp + clp, php + chp), a, b)) (pushButton x)) ((0, 0), False, m)

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution1 <$> input

solution2 m = go 1 (pushButton m)
  where
    go c (_, True, _) = c
    go c (_, False, m') = go (c + 1) (pushButton m')

solution2' m = (\(c, _, _) -> c) $ until (\(_, b, _) -> b) (\(c, _, x) -> (\(_, a, b) -> ((c + 1), a, b)) (pushButton x)) (0, False, m)

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = solution2' <$> input

testInput :: Modules
testInput =
    parseInput
        "broadcaster -> a, b, c\n\
        \%a -> b\n\
        \%b -> c\n\
        \%c -> inv\n\
        \&inv -> a"

testInput2 :: Modules
testInput2 =
    parseInput
        "broadcaster -> a\n\
        \%a -> inv, con\n\
        \&inv -> b\n\
        \%b -> con\n\
        \&con -> output"
