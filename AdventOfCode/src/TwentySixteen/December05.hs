module TwentySixteen.December05 where

import Control.Monad (mfilter)
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as Vector (
    all,
    filter,
    foldl,
    generate,
    indexed,
    length,
    map,
    (//),
 )
import Lib.MD5 (generateMD5)
import Text.Read (readMaybe)

input :: IO String
input = init <$> readFile "input/2016/5December.txt"

inputTest :: String
inputTest = "abc"

findPassword :: String -> [Int] -> Int -> String
findPassword _ _ 0 = ""
findPassword prefix (x : xs) countdown
    | all ('0' ==) (take 5 md5) =
        md5 !! 5 : findPassword prefix xs (countdown - 1)
    | otherwise = findPassword prefix xs countdown
  where
    md5 = generateMD5 $ prefix ++ show x

solution1 :: String -> String
solution1 prefix = findPassword prefix [0 ..] 8

solution1Test :: Bool
solution1Test = solution1 inputTest == "18f47a30"

december05Solution1 :: IO String
december05Solution1 = solution1 <$> input

findPasswordWithPosition :: String -> [Int] -> Vector Char -> String
findPasswordWithPosition prefix (x : xs) result
    | Vector.all ('_' /=) result = Vector.foldl (\acc c -> acc ++ [c]) [] result
    | all ('0' ==) (take 5 md5) && isJust maybeIndex =
        findPasswordWithPosition
            prefix
            xs
            ((Vector.//) result [(fromJust maybeIndex, md5 !! 6)])
    | otherwise = findPasswordWithPosition prefix xs result
  where
    md5 = generateMD5 $ prefix ++ show x
    maybeIndex =
        mfilter
            ( `elem`
                ( Vector.map fst
                    . Vector.filter (('_' ==) . snd)
                    . Vector.indexed
                )
                    result
            )
            (readMaybe [md5 !! 5] :: Maybe Int)

solution2 :: String -> String
solution2 prefix =
    findPasswordWithPosition prefix [0 ..] (Vector.generate 8 (const '_'))

solution2Test :: Bool
solution2Test = solution2 inputTest == "05ace8e3"

december05Solution2 :: IO String
december05Solution2 = solution2 <$> input
