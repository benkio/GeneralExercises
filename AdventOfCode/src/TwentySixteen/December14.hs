module TwentySixteen.December14 where

import qualified Data.ByteString.Lazy.Char8 as B
import Lib.MD5 (generateMD5, generateMD5WithPrefix)
import Data.List
import Data.Maybe

mD5s :: String -> [String]
mD5s prefix = fmap (generateMD5WithPrefix prefix . show) [1 ..]

isKey :: String -> [String] -> Bool
isKey candidate nextMD5
    | isJust maybeTriples
        && not
            ( any
                ( any
                    (\l -> elem (head l) (fromJust maybeTriples) && length l >= 5)
                    . group
                )
                nextMD5
            ) =
        True
    | otherwise = False
  where
    maybeTriples = (find ((3 <=) . length) . group) candidate

input :: IO String
input = init <$> readFile "input/2016/14December.txt"

generateValidKeys ::
    (String -> String) -> Int -> Int -> [String] -> [Int]
generateValidKeys _ _ 0 _ = []
generateValidKeys genMD5 step numOfKeys nextMD5
    | isKey (genMD5 (show step)) nextMD5 =
        step : generateValidKeys genMD5 nextStep (numOfKeys - 1) nextMD5'
    | otherwise = generateValidKeys genMD5 nextStep numOfKeys nextMD5'
  where
    nextStep = step + 1
    nextMD5' = ((++ [genMD5 (show (nextStep + 1000))]) . tail) nextMD5

inputTest :: Bool
inputTest =
    last (generateValidKeys (generateMD5WithPrefix "abc") 0 64 (take 1000 (mD5s "abc")))
        == 22728

december14Solution1 :: IO Int
december14Solution1 =
    last . (\x -> generateValidKeys (generateMD5WithPrefix x) 0 64 (take 1000 (mD5s x)))
        <$> input

generateMD52016 :: String -> String -> String
generateMD52016 prefix i =
    iterate (generateMD5) (generateMD5WithPrefix prefix i) !! 2016

mD52016s :: String -> [String]
mD52016s prefix = fmap (generateMD52016 prefix . show) [1 ..]

december14Solution2 :: IO Int
december14Solution2 =
    last
        . (\x -> generateValidKeys (generateMD52016 x) 0 64 (take 1000 (mD52016s x)))
        <$> input

inputTest2 :: Bool
inputTest2 =
    last
        (generateValidKeys (generateMD52016 "abc") 0 64 (take 1000 (mD52016s "abc")))
        == 22551
