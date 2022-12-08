module TwentyTwentyOne.EighthDecember where

import Data.Bifunctor (bimap)
import Data.List (find, sort, sortBy, (\\))
import Data.Map (Map)
import qualified Data.Map as M (empty, fromList, insert, lookup, size, toList, union)
import Data.Maybe (fromJust)

type Input = ([String], [String])

input :: IO [Input]
input = parseInput <$> readFile "input/2021/8December.txt"

parseInput :: String -> [Input]
parseInput =
  fmap
    ( bimap
        (sortBy (\s s' -> length s `compare` length s') . fmap sort . words)
        (fmap sort . words . drop 2)
        . break (== '|')
    )
    . lines

numSegToRealSeg :: Int -> [Int]
numSegToRealSeg 2 = [1]
numSegToRealSeg 3 = [7]
numSegToRealSeg 4 = [4]
numSegToRealSeg 5 = [2, 3, 5]
numSegToRealSeg 6 = [0, 9, 6]
numSegToRealSeg 7 = [8]

buildDictionary :: Map String Int -> [String] -> (Map String Int, [String])
buildDictionary dic (x : xs) =
  let candidates = numSegToRealSeg (length x)
      (newDic, rest) = if length candidates == 1 then (M.insert x (head candidates) dic, xs) else (dic, xs ++ [x])
   in if M.size newDic == 4 then (newDic, rest) else buildDictionary newDic rest

findMagicNumber :: Map String Int -> [String] -> [Int]
findMagicNumber _ [] = []
findMagicNumber dic (x : xs) = foldr const (-1) (M.lookup x dic) : findMagicNumber dic xs

solution1 :: (Map String Int -> [String] -> Map String Int) -> Input -> Int
solution1 f (digitsRaw, magicNumberRaw) =
  let dic = f M.empty digitsRaw
      magicNumbers = findMagicNumber dic magicNumberRaw
   in (length . filter (>= 0)) magicNumbers

eighthDecemberSolution1 :: IO Int
eighthDecemberSolution1 = sum . fmap (solution1 (\d i -> fst (buildDictionary d i))) <$> input

buildDictionary' :: [String] -> Map String Int
buildDictionary' is =
  let (dic, xs) = buildDictionary M.empty is
      dicList = M.toList dic
      seven = (fst . fromJust . find (\(_, i) -> i == 7)) dicList
      four = (fst . fromJust . find (\(_, i) -> i == 4)) dicList
      three = (fromJust . find (\x -> length x == 5 && null (seven \\ x))) xs
      nine = (fromJust . find (\x -> length x == 6 && null (four \\ x))) xs

      xs' = filter (\x -> x /= three && x /= nine) xs
      zero = (fromJust . find (\x -> length x == 6 && null (seven \\ x))) xs'
      six = (head . filter (\x -> length x == 6 && x /= zero)) xs'

      xs'' = filter (\x -> x /= zero && x /= six) xs'
      five = (fromJust . find (\x -> length x == 5 && null (x \\ six))) xs''
      two = (head . filter (/= five)) xs''

      newDic = dic `M.union` M.fromList [(three, 3), (nine, 9), (zero, 0), (six, 6), (five, 5), (two, 2)]
   in newDic

solution2 :: ([String] -> Map String Int) -> Input -> Int
solution2 f (digitsRaw, magicNumberRaw) =
  let dic = f digitsRaw
      magicNumbers = findMagicNumber dic magicNumberRaw
   in (sum . zipWith (\i x -> 10 ^ i * x) [0 ..] . reverse) magicNumbers

eighthDecemberSolution2 :: IO Int
eighthDecemberSolution2 = sum . fmap (solution2 buildDictionary') <$> input

inputTest :: [Input]
inputTest =
  parseInput
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
    \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
    \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
    \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
    \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
    \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
    \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
    \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
    \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
    \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
