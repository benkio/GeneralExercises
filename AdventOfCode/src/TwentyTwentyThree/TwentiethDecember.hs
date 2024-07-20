module TwentyTwentyThree.TwentiethDecember where

import Data.Bifunctor (first)
import Data.List (isPrefixOf)
import Data.Map (Map, adjust, empty, fromList, insert, toList)
import qualified Data.Map as Map (foldr, lookup)
import Data.Maybe (fromJust, isNothing)

data Pulse = P Bool String String deriving (Show)
type Modules = Map String Module
data Module
    = FF String Bool [String]
    | C String (Map String Bool) [String]
    | BR [String]
    deriving (Show)
data ButtonResult = ButtonResult
    { modules :: Modules
    , highPulses :: Int
    , lowPulses :: Int
    , buttonPushNum :: Int
    , ddTrigger :: Bool
    , fhTrigger :: Bool
    , xpTrigger :: Bool
    , fcTrigger :: Bool
    }
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
         in if all snd mem'
                then (c', fmap (P False s) out)
                else (c', fmap (P True s) out)
    processPulse m@(BR ds) p = (m, fmap (setPulseSrc "broadcaster" . (`setPulseDst` p)) ds)

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
        foldl (flip (adjust (`addInput` getModuleKey x))) m' (getModuleOutput x)

initialButtonResult m =
    ButtonResult
        { modules = m
        , highPulses = 0
        , lowPulses = 0
        , buttonPushNum = 0
        , ddTrigger = False
        , fhTrigger = False
        , xpTrigger = False
        , fcTrigger = False
        }

pushButton :: ButtonResult -> ButtonResult
pushButton br = go br{lowPulses = lowPulses br + 1, buttonPushNum = buttonPushNum br + 1} $ snd $ processPulse broacastModule (P False "aptly" "broadcaster")
  where
    broacastModule = fromJust $ Map.lookup "broadcaster" (modules br)
    updatePulsesCounter br@(ButtonResult{highPulses = hp}) (P True _ _) = br{highPulses = hp + 1}
    updatePulsesCounter br@(ButtonResult{lowPulses = lp}) (P False _ _) = br{lowPulses = lp + 1}
    activateModule md v (P pv src dst) = src == md && pv == v && dst == "dn"
    checkModules x p
        | activateModule "dd" True p = x{ddTrigger = True}
        | activateModule "fh" True p = x{fhTrigger = True}
        | activateModule "xp" True p = x{xpTrigger = True}
        | activateModule "fc" True p = x{fcTrigger = True}
        | otherwise = x
    go :: ButtonResult -> [Pulse] -> ButtonResult
    go br [] = br
    go br@(ButtonResult{modules = m}) (p : ps)
        | isNothing (Map.lookup (getPulseDst p) m) =
            go (((`checkModules` p) . updatePulsesCounter br) p) ps
        | otherwise =
            let
                pDst = getPulseDst p
                modul = fromJust $ Map.lookup pDst m
                (modul', pulses) = processPulse modul p
                m' = insert pDst modul' m
                br' = (checkModules (updatePulsesCounter br p) p){modules = m'}
             in
                go br' (ps ++ pulses)

solution1 :: Modules -> Int
solution1 =
    (\br -> lowPulses br * highPulses br)
        . (!! 1000)
        . iterate pushButton
        . initialButtonResult

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = solution1 <$> input

solution2 m = foldl1 lcm [ddHighPulse, fhHighPulse, xpHighPulse, fcHighPulse]
  where
    buttonResult = initialButtonResult m
    ddHighPulse = findHighPulseLoopForModule "dd" buttonResult
    fhHighPulse = findHighPulseLoopForModule "fh" buttonResult
    xpHighPulse = findHighPulseLoopForModule "xp" buttonResult
    fcHighPulse = findHighPulseLoopForModule "fc" buttonResult

findHighPulseLoopForModule :: String -> ButtonResult -> Int
findHighPulseLoopForModule mid br = buttonPushNum secondHighPulseButtonResult
  where
    firstHighPulseButtonResult = findHighPulse br
    secondHighPulseButtonResult = findHighPulse ((initialButtonResult . modules) firstHighPulseButtonResult)
    findHighPulse =
        until
            ( \x -> case mid of
                "dd" -> ddTrigger x
                "fh" -> fhTrigger x
                "xp" -> xpTrigger x
                "fc" -> fcTrigger x
            )
            pushButton

-- twentiethDecemberSolution2 :: IO Int
-- twentiethDecemberSolution2 = solution2 <$> input

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
