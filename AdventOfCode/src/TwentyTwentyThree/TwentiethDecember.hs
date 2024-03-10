{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.TwentiethDecember where

import Data.List (isPrefixOf)
import Data.Map (Map, empty, fromList)

data Pulse = P Bool String String deriving (Show)
type Modules = Map String Module
data Module
    = FF String Bool [String]
    | C String (Map String Bool) [String]
    | BR [String]
    deriving (Show)

moduleKey :: Module -> String
moduleKey (FF k _ _) = k
moduleKey (C k _ _) = k
moduleKey (BR _) = "broadcaster"

class ProcessPulse a where
    processPulse :: a -> Pulse -> (a, [Pulse])

instance ProcessPulse Module where
    processPulse m@(FF _ s ds) (P True _ _) = (m, [])
    processPulse (FF k s ds) p@(P False _ _) = (FF k (not s) ds, fmap (setPulseSrc k . setPulseValue (not s) . (`setPulseDst` p)) ds)
    processPulse (C _ _ mem) p@(P v _ d) = undefined
    processPulse m@(BR ds) p = (m, (fmap (setPulseSrc "broadcaster" . (`setPulseDst` p)) ds))

setPulseValue :: Bool -> Pulse -> Pulse
setPulseValue b (P _ s d) = P b s d

setPulseSrc :: String -> Pulse -> Pulse
setPulseSrc src (P v _ d) = P v src d

setPulseDst :: String -> Pulse -> Pulse
setPulseDst dst (P v s _) = P v s dst

input :: IO Modules
input = parseInput <$> readFile "input/2023/20December.txt"

parseInput :: String -> Modules
parseInput = updateConjunctionInputs . fromList . fmap ((\m -> (moduleKey m, m)) . parseModule) . lines
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
updateConjunctionInputs = undefined

testInput :: Modules
testInput =
    parseInput
        "broadcaster -> a, b, c\n\
        \%a -> b\n\
        \%b -> c\n\
        \%c -> inv\n\
        \&inv -> a"

solution1 = undefined

twentiethDecemberSolution1 :: IO Int
twentiethDecemberSolution1 = undefined

solution2 = undefined

twentiethDecemberSolution2 :: IO Int
twentiethDecemberSolution2 = undefined
