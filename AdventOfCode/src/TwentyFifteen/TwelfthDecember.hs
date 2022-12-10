module TwentyFifteen.TwelfthDecember where

-- https://stackoverflow.com/questions/23098405/haskell-parsing-json-data-into-a-map-or-a-list-of-tuples/23098515
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe
import Data.Scientific
import Data.Text as Text (unpack)

input :: IO (Maybe Object)
input = parseToObject <$> readFile "input/2015/12December.txt"

parseToObject :: String -> Maybe Object
parseToObject = decode . BS.pack

inputTest :: [(Object, Object, Int)]
inputTest =
    [
        ( fromJust (parseToObject "{\"a\":[1,2,3]}")
        , fromJust (parseToObject "{\"a\":2,\"b\":4}")
        , 6
        )
    ,
        ( fromJust (parseToObject "{\"a\": [[[3]]]}")
        , fromJust (parseToObject "{\"a\":{\"b\":4},\"c\":-1}")
        , 3
        )
    ,
        ( fromJust (parseToObject "{\"a\":[-1,1]}")
        , fromJust (parseToObject "{\"a\":[-1,{\"a\":1}]}")
        , 0
        )
    , (fromJust (parseToObject "{\"a\":[]}"), fromJust (parseToObject "{}"), 0)
    ]

foldValue :: Int -> Value -> (Object -> Bool) -> (Object -> Int) -> Int
foldValue acc (Object o) objectCheck computeObject
    | objectCheck o = acc
    | otherwise = acc + computeObject o
foldValue acc (Array v) objectCheck computeObject =
    acc + (sum . fmap (\x -> foldValue 0 x objectCheck computeObject)) v
foldValue acc (Number s) _ _ = acc + fromJust (toBoundedInteger s)
foldValue acc (String _) _ _ = acc
foldValue acc (Bool _) _ _ = acc
foldValue acc Null _ _ = acc

solution1 :: Object -> Int
solution1 = foldl (\acc x -> foldValue acc x (const False) solution1) 0

solution1Test :: Bool
solution1Test =
    all
        ( \(o, o', expected) ->
            solution1 o == solution1 o' && solution1 o' == expected
        )
        inputTest

isRedObject :: Object -> Bool
isRedObject = foldl isRedValue False
  where
    isRedValue :: Bool -> Value -> Bool
    isRedValue False (String s) = Text.unpack s == "red"
    isRedValue x _ = x

solution2Test :: Bool
solution2Test = all (\(o, expected) -> solution2 o == expected) inputTest2

inputTest2 :: [(Object, Int)]
inputTest2 =
    [ (fromJust (parseToObject "{\"a\":[1,2,3]}"), 6)
    , (fromJust (parseToObject "{\"a\": [1,{\"c\":\"red\",\"b\":2},3]}"), 4)
    , (fromJust (parseToObject "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"), 0)
    , (fromJust (parseToObject "{\"a\":[1,\"red\",5]}"), 6)
    ]

solution2 :: Object -> Int
solution2 o
    | isRedObject o = 0
    | otherwise = foldl (\acc x -> foldValue acc x isRedObject solution2) 0 o

twelfthDecemberSolution1 :: IO Int
twelfthDecemberSolution1 = solution1 . fromJust <$> input

twelfthDecemberSolution2 :: IO Int
twelfthDecemberSolution2 = solution2 . fromJust <$> input
