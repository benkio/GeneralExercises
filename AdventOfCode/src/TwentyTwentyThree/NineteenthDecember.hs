{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.NineteenthDecember where

import Data.List (find, findIndex)

import Data.Maybe (catMaybes, fromJust, isJust)

import Data.Bifunctor (bimap)

import Data.List (break)
import GHC.Utils.Misc (split)

data MachinePart = MP
    { mp_x :: Int
    , mp_m :: Int
    , mp_a :: Int
    , mp_s :: Int
    }
    deriving (Show)

data Flow = F {fId :: String, checks :: [Check]} deriving (Show)
data Check = C (MachinePart -> Maybe String)

instance Show Check where
    show _ = "C"

machinePartValueFromString :: String -> MachinePart -> Int
machinePartValueFromString "x" = mp_x
machinePartValueFromString "m" = mp_m
machinePartValueFromString "a" = mp_a
machinePartValueFromString "s" = mp_s

machinePartRating :: MachinePart -> Int
machinePartRating m =
    mp_x m
        + mp_m m
        + mp_a m
        + mp_s m

applyCheck :: Check -> MachinePart -> Maybe String
applyCheck (C f) m = f m

applyFlow :: Flow -> MachinePart -> String
applyFlow (F{checks = cs}) m = head . catMaybes . fmap ((flip applyCheck) m) $ cs

input :: IO ([Flow], [MachinePart])
input = parseInput <$> readFile "input/2023/19December.txt"

filterAcceptedMachineParts :: [Flow] -> [(MachinePart, String)] -> [MachinePart]
filterAcceptedMachineParts fs [] = []
filterAcceptedMachineParts fs ((m, s) : ms)
    | s == "R" = filterAcceptedMachineParts fs ms
    | s == "A" = m : filterAcceptedMachineParts fs ms
    | otherwise = filterAcceptedMachineParts fs $ ms ++ [(m, applyFlow f m)]
  where
    f = fromJust $ find ((== s) . fId) fs

solution1 (fs, ms) = sum . fmap machinePartRating $ filterAcceptedMachineParts fs (fmap (,"in") ms)

ninetheenthDecemberSolution1 :: IO Int
ninetheenthDecemberSolution1 = solution1 <$> input

-- I crate 4 ranges, 1-4000 foreach category
-- I consume the flow by sectioning the ranges by the threshold and bind the n outcomes to the new flows.
-- Eventually all the ranges will have an A or an R
-- Keep the As, sum by category, multiply the sums
-- Profit

data MetaMachinePart = MMP
  { mmp_x :: (Int,Int)
    , mmp_m :: (Int,Int)
    , mmp_a :: (Int,Int)
    , mmp_s :: (Int,Int)
    }

mmp :: MetaMachinePart
mmp = MMP { mmp_x = (1,4000)
    , mmp_m = (1,4000)
    , mmp_a = (1,4000)
    , mmp_s = (1,4000)
    }

metaApplyCheck :: Check -> MetaMachinePart -> Maybe String
metaApplyCheck (C f) m = f m

metaApplyFlow :: Flow -> MetaMachinePart -> String
metaApplyFlow (F{checks = cs}) m = head . catMaybes . fmap ((flip applyCheck) m) $ cs


solution2 = undefined

ninetheenthDecemberSolution2 :: IO Int
ninetheenthDecemberSolution2 = undefined

parseMachinePart :: String -> MachinePart
parseMachinePart i = MP{mp_x = xv, mp_m = mv, mp_a = av, mp_s = sv}
  where
    dropTillNum = tail . dropWhile (/= '=')
    extractNumber n l s = (\b -> read b :: Int) . takeWhile (/= l) . foldl (\x f -> f x) s . take n $ repeat dropTillNum
    xv = extractNumber 1 ',' i
    mv = extractNumber 2 ',' i
    av = extractNumber 3 ',' i
    sv = extractNumber 4 '}' i

parseCheck :: String -> Check
parseCheck s
    | '<' `elem` s = C (\m -> if machinePartValueFromString (valueString '<' s) m < valueCheck '<' s then Just (valueFlow s) else Nothing)
    | '>' `elem` s = C (\m -> if machinePartValueFromString (valueString '>' s) m > valueCheck '>' s then Just (valueFlow s) else Nothing)
    | otherwise = C (const $ Just s)
  where
    valueString c = takeWhile (/= c)
    valueCheck c = (\x -> read x :: Int) . takeWhile (/= ':') . tail . dropWhile (/= c)
    valueFlow = tail . dropWhile (/= ':')

parseFlow :: String -> Flow
parseFlow s = F{fId = fIdValue, checks = cs}
  where
    fIdValue = takeWhile (/= '{') s
    cs = fmap parseCheck . split ',' . init . tail . dropWhile (/= '{') $ s

parseInput :: String -> ([Flow], [MachinePart])
parseInput = bimap (fmap parseFlow) (fmap parseMachinePart . tail) . break (== "") . lines

testInput :: ([Flow], [MachinePart])
testInput =
    parseInput
        "px{a<2006:qkq,m>2090:A,rfg}\n\
        \pv{a>1716:R,A}\n\
        \lnx{m>1548:A,A}\n\
        \rfg{s<537:gd,x>2440:R,A}\n\
        \qs{s>3448:A,lnx}\n\
        \qkq{x<1416:A,crn}\n\
        \crn{x>2662:A,R}\n\
        \in{s<1351:px,qqz}\n\
        \qqz{s>2770:qs,m<1801:hdj,R}\n\
        \gd{a>3333:R,R}\n\
        \hdj{m>838:A,pv}\n\
        \\n\
        \{x=787,m=2655,a=1222,s=2876}\n\
        \{x=1679,m=44,a=2067,s=496}\n\
        \{x=2036,m=264,a=79,s=2244}\n\
        \{x=2461,m=1339,a=466,s=291}\n\
        \{x=2127,m=1623,a=2188,s=1013}"
