{-# LANGUAGE TupleSections #-}

module TwentyTwentyThree.NineteenthDecember where

import Data.Bifunctor (bimap)
import Data.List (break, find, findIndex)
import Data.Maybe (mapMaybe, fromJust, isJust)
import GHC.Utils.Misc (split)

data MachinePart = MP
    { mp_x :: Int
    , mp_m :: Int
    , mp_a :: Int
    , mp_s :: Int
    }
    deriving (Show)

data Flow = F {fId :: String, checks :: [Check]} deriving (Show)
newtype Check = C (MachinePart -> Maybe String)

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
applyCheck (C f) = f

applyFlow :: Flow -> MachinePart -> String
applyFlow (F{checks = cs}) m = head . mapMaybe (`applyCheck` m) $ cs

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

parseMachinePart :: String -> MachinePart
parseMachinePart i = MP{mp_x = xv, mp_m = mv, mp_a = av, mp_s = sv}
  where
    dropTillNum = tail . dropWhile (/= '=')
    extractNumber n l s = ((\ b -> read b :: Int) . takeWhile (/= l) . foldl (\ dropTillNum f -> f dropTillNum) s) (replicate n dropTillNum)
    xv = extractNumber 1 ',' i
    mv = extractNumber 2 ',' i
    av = extractNumber 3 ',' i
    sv = extractNumber 4 '}' i

parseCheck :: String -> Check
parseCheck s
    | '<' `elem` s = C (\m -> if machinePartValueFromString (valueString '<' s) m < valueCheck '<' s then Just valueFlow else Nothing)
    | '>' `elem` s = C (\m -> if machinePartValueFromString (valueString '>' s) m > valueCheck '>' s then Just valueFlow else Nothing)
    | otherwise = C (const $ Just s)
  where
    valueString c = takeWhile (/= c)
    valueCheck c = (\x -> read x :: Int) . takeWhile (/= ':') . tail . dropWhile (/= c)
    valueFlow = tail . dropWhile (/= ':') $ s

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

-- I crate 4 ranges, 1-4000 foreach category
-- I consume the flow by sectioning the ranges by the threshold and bind the n outcomes to the new flows.
-- Eventually all the ranges will have an A or an R
-- Keep the As, sum by category, multiply the sums
-- Profit

input2 :: IO [MetaFlow]
input2 = parseInput2 <$> readFile "input/2023/19December.txt"

data MetaMachinePart = MMP
    { mmp_x :: (Int, Int)
    , mmp_m :: (Int, Int)
    , mmp_a :: (Int, Int)
    , mmp_s :: (Int, Int)
    }
    deriving (Show)
data MetaFlow = MF {mfId :: String, mChecks :: [MetaCheck]} deriving (Show)
data Ior a b = L a | R b | B (a, b)
newtype MetaCheck = MC (MetaMachinePart -> Ior (MetaMachinePart, String) MetaMachinePart)

instance Show MetaCheck where
    show _ = "MC"

mmp :: MetaMachinePart
mmp =
    MMP
        { mmp_x = (1, 4000)
        , mmp_m = (1, 4000)
        , mmp_a = (1, 4000)
        , mmp_s = (1, 4000)
        }

metaMachinePartValueFromString :: String -> MetaMachinePart -> (Int, Int)
metaMachinePartValueFromString "x" = mmp_x
metaMachinePartValueFromString "m" = mmp_m
metaMachinePartValueFromString "a" = mmp_a
metaMachinePartValueFromString "s" = mmp_s

setMetaMachinePart :: (Int, Int) -> String -> MetaMachinePart -> MetaMachinePart
setMetaMachinePart v "x" mmp = mmp{mmp_x = v}
setMetaMachinePart v "m" mmp = mmp{mmp_m = v}
setMetaMachinePart v "a" mmp = mmp{mmp_a = v}
setMetaMachinePart v "s" mmp = mmp{mmp_s = v}

metaApplyCheck :: MetaCheck -> MetaMachinePart -> Ior (MetaMachinePart, String) MetaMachinePart
metaApplyCheck (MC f) = f

metaApplyFlow :: MetaFlow -> MetaMachinePart -> [(MetaMachinePart, String)]
metaApplyFlow (MF{mChecks = cs}) m = fst . foldl foldlF ([], m) $ cs
  where
    foldlF :: ([(MetaMachinePart, String)], MetaMachinePart) -> MetaCheck -> ([(MetaMachinePart, String)], MetaMachinePart)
    foldlF (acc, mmp) mc = case metaApplyCheck mc mmp of
        L a -> (acc ++ [a], error "this should never be evaluated")
        R b -> (acc, b)
        B (a, b) -> (acc ++ [a], b)

filterAcceptedMetaMachineParts :: [MetaFlow] -> [(MetaMachinePart, String)] -> [MetaMachinePart]
filterAcceptedMetaMachineParts fs [] = []
filterAcceptedMetaMachineParts fs ((m, s) : ms)
    | s == "R" = filterAcceptedMetaMachineParts fs ms
    | s == "A" = m : filterAcceptedMetaMachineParts fs ms
    | otherwise = filterAcceptedMetaMachineParts fs $ ms ++ metaApplyFlow f m
  where
    f = fromJust $ find ((== s) . mfId) fs

metaMachinePartRating :: MetaMachinePart -> Int
metaMachinePartRating MMP{mmp_x = (minxv, maxxv), mmp_m = (minmv, maxmv), mmp_a = (minav, maxav), mmp_s = (minsv, maxsv)} =
    length [minxv .. maxxv] * length [minmv .. maxmv] * length [minav .. maxav] * length [minsv .. maxsv]

solution2 fs =
    sum
        . fmap metaMachinePartRating
        $ filterAcceptedMetaMachineParts fs [(mmp, "in")]

ninetheenthDecemberSolution2 :: IO Int
ninetheenthDecemberSolution2 = solution2 <$> input2

parseMetaFlow :: String -> MetaFlow
parseMetaFlow s = MF{mfId = fIdValue, mChecks = cs}
  where
    fIdValue = takeWhile (/= '{') s
    cs = fmap parseMetaCheck . split ',' . init . tail . dropWhile (/= '{') $ s

parseMetaCheck :: String -> MetaCheck
parseMetaCheck s
    | '<' `elem` s = MC (metaCheckLogic '<')
    | '>' `elem` s = MC (metaCheckLogic '>')
    | otherwise = MC (\mmp -> L (mmp, s))
  where
    valueString c = takeWhile (/= c)
    valueCheck c = (\x -> read x :: Int) . takeWhile (/= ':') . tail . dropWhile (/= c)
    valueFlow = tail . dropWhile (/= ':') $ s
    metaCheckLogic :: Char -> (MetaMachinePart -> Ior (MetaMachinePart, String) MetaMachinePart)
    metaCheckLogic c mmp =
        let
            selector = valueString c s
            (minV, maxV) = metaMachinePartValueFromString selector mmp
            tv = valueCheck c s
         in
            buildMetaMachinePart c selector tv (minV, maxV) mmp
    buildMetaMachinePart :: Char -> String -> Int -> (Int, Int) -> MetaMachinePart -> Ior (MetaMachinePart, String) MetaMachinePart
    buildMetaMachinePart '<' selector tv (minV, maxV) mmp =
        case (tv < minV, tv > minV && tv < maxV, tv > maxV) of
            (True, _, _) -> R mmp
            (_, True, _) -> B ((setMetaMachinePart (minV, tv - 1) selector mmp, valueFlow), setMetaMachinePart (tv, maxV) selector mmp)
            (_, _, True) -> L (mmp, valueFlow)
    buildMetaMachinePart '>' selector tv (minV, maxV) mmp =
        case (tv < minV, tv > minV && tv < maxV, tv > maxV) of
            (True, _, _) -> L (mmp, valueFlow)
            (_, True, _) -> B ((setMetaMachinePart (tv + 1, maxV) selector mmp, valueFlow), setMetaMachinePart (minV, tv) selector mmp)
            (_, _, True) -> R mmp

parseInput2 :: String -> [MetaFlow]
parseInput2 = fmap parseMetaFlow . takeWhile (/= "") . lines

testInput2 :: [MetaFlow]
testInput2 =
    parseInput2
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

-- controlled test ----------------------------------------------------
-- result 1900

mmp' =
    MMP
        { mmp_x = (1, 10)
        , mmp_m = (1, 10)
        , mmp_a = (1, 10)
        , mmp_s = (1, 10)
        }

test fs =
    -- sum .
    -- fmap metaMachinePartRating $
    filterAcceptedMetaMachineParts fs [(mmp', "in")]

testInput3 :: [MetaFlow]
testInput3 =
    parseInput2
        "in{s<5:a,b}\n\
        \a{s>3:A,m<4:A,R}\n\
        \b{a>7:R,R}\n\
        \\n\
        \{x=787,m=2655,a=1222,s=2876}\n\
        \{x=1679,m=44,a=2067,s=496}\n\
        \{x=2036,m=264,a=79,s=2244}\n\
        \{x=2461,m=1339,a=466,s=291}\n\
        \{x=2127,m=1623,a=2188,s=1013}"
