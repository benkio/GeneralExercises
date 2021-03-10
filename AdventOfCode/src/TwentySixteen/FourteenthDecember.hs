module TwentySixteen.FourteenthDecember where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Digest.Pure.MD5 as M
import Data.List
import Data.Maybe

mD5s :: String -> [String]
mD5s prefix = fmap (generateMD5 prefix . show) [1 ..]

generateMD5 :: String -> String -> String
generateMD5 prefix num =
  let md5input = B.pack $ prefix ++ num
   in show $ M.md5 md5input

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
  (String -> String -> String) -> Int -> Int -> String -> [String] -> [Int]
generateValidKeys _ _ 0 _ _ = []
generateValidKeys genMD5 step numOfKeys prefix nextMD5
  | isKey (genMD5 prefix (show step)) nextMD5 =
    step : generateValidKeys genMD5 nextStep (numOfKeys - 1) prefix nextMD5'
  | otherwise = generateValidKeys genMD5 nextStep numOfKeys prefix nextMD5'
  where
    nextStep = step + 1
    nextMD5' = ((++ [genMD5 prefix (show (nextStep + 1000))]) . tail) nextMD5

inputTest :: Bool
inputTest =
  last (generateValidKeys generateMD5 0 64 "abc" (take 1000 (mD5s "abc")))
    == 22728

fourteenthDecemberSolution1 :: IO Int
fourteenthDecemberSolution1 =
  last . (\x -> generateValidKeys generateMD5 0 64 x (take 1000 (mD5s x)))
    <$> input

generateMD52016 :: String -> String -> String
generateMD52016 prefix i =
  iterate (show . M.md5 . B.pack) (generateMD5 prefix i) !! 2016

mD52016s :: String -> [String]
mD52016s prefix = fmap (generateMD52016 prefix . show) [1 ..]

fourteenthDecemberSolution2 :: IO Int
fourteenthDecemberSolution2 =
  last
    . (\x -> generateValidKeys generateMD52016 0 64 x (take 1000 (mD52016s x)))
    <$> input

inputTest2 :: Bool
inputTest2 =
  last
    (generateValidKeys generateMD52016 0 64 "abc" (take 1000 (mD52016s "abc")))
    == 22551
