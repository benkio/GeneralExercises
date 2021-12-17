module TwentyTwentyOne.SixteenthDecember where

import Data.Map (Map, (!))
import qualified Data.Map as M (fromList)

data Header = Header {version :: String, typeId :: String} deriving (Show)

data Packet
  = LiteralValue
      { header :: Header,
        value :: String
      }
  | Operator
      { header :: Header,
        subPackets :: [Packet]
      }
  deriving (Show)

data LengthTypeId = TotalLength String | SubPackets Int deriving (Show)

hexToBitsMap :: Map Char String
hexToBitsMap = M.fromList [('0', "0000"), ('1', "0001"), ('2', "0010"), ('3', "0011"), ('4', "0100"), ('5', "0101"), ('6', "0110"), ('7', "0111"), ('8', "1000"), ('9', "1001"), ('A', "1010"), ('B', "1011"), ('C', "1100"), ('D', "1101"), ('E', "1110"), ('F', "1111")]

hexToBits :: String -> String
hexToBits = foldl (\acc x -> acc ++ hexToBitsMap ! x) []

class Parsable a where
  parse :: String -> (a, String)

instance Parsable Header where
  parse s = (Header {version = take 3 s, typeId = (take 3 . drop 3) s}, drop 6 s)

instance Parsable Packet where
  parse s =
    let (header, s') = parse s :: (Header, String)
     in if isLiteralPackage header
          then (LiteralValue {header = header, value = fst (parseLiteralValue 6 s')}, snd (parseLiteralValue 6 s'))
          else case parseLengthTypeId s' of
            (TotalLength c, s'') ->
              let (_, packages) =
                    until
                      (\(x, _) -> null x)
                      ( \(i, ps) ->
                          let (p, i'') = parse i :: (Packet, String)
                           in (i'', ps ++ [p])
                          --((\(p, i'') -> (i'', ps ++ [p])) . parse) i
                      )
                      (c, [])
               in (Operator {header = header, subPackets = packages}, s'')
            (SubPackets n, s'') ->
              let (s''', _, packages) = until (\(_, x, _) -> x == 0) (\(i, c, ps) -> ((\(p, i'') -> (i'', c - 1, ps ++ [p])) . parse) i) (s'', n, [])
               in (Operator {header = header, subPackets = packages}, s''')

isLiteralPackage :: Header -> Bool
isLiteralPackage Header {typeId = t} = t == "100"

parseLiteralValue :: Int -> String -> (String, String)
parseLiteralValue c [] = error "parseLiteralValue: Unreachable, expected a value"
parseLiteralValue c s
  | head s == '1' =
    let (nxtV, s') = parseLiteralValue (c + 5) (drop 5 s)
     in (currentValue ++ nxtV, s')
  | head s == '0' = (currentValue, drop 5 s) -- drop (5 + (4 - rem (c + 5) 4)) s)
  | otherwise = error "parseLiteralValue: Expected a binary value"
  where
    currentValue = (take 4 . drop 1) s

binaryToNum :: String -> Int
binaryToNum = foldl (\n (e, v) -> n + (2 ^ e * (read [v] :: Int))) 0 . zip [0 ..] . reverse

parseLengthTypeId :: String -> (LengthTypeId, String)
parseLengthTypeId [] = error "parseLengthTypeId: expected a length type id get an empty string"
parseLengthTypeId ('0' : xs) =
  let l = (binaryToNum . take 15) xs
      xs' = (take l . drop 15) xs
   in (TotalLength xs', drop (l + 15) xs)
parseLengthTypeId ('1' : xs) =
  let l = (binaryToNum . take 11) xs
   in (SubPackets l, drop 11 xs)

sumVersionNums :: Packet -> Int
sumVersionNums LiteralValue {header = Header {version = v}} = binaryToNum v
sumVersionNums Operator {header = Header {version = v}, subPackets = ps} = binaryToNum v + sum (fmap sumVersionNums ps)

solution :: (Packet -> Int) -> String -> Int
solution f = f . fst . (\x -> parse x :: (Packet, String)) . hexToBits

input :: IO String
input = readFile "input/2021/16December.txt"

inputTest1 :: String
inputTest1 = "8A004A801A8002F478"

inputTest2 :: String
inputTest2 = "620080001611562C8802118E34"

inputTest3 :: String
inputTest3 = "C0015000016115A2E0802F182340"

inputTest4 :: String
inputTest4 = "A0016C880162017C3686B18A3D4780"

sixteenthDecemberSolution1 :: IO Int
sixteenthDecemberSolution1 = solution sumVersionNums <$> input

evaluate :: Packet -> Int
evaluate LiteralValue {value = v} = binaryToNum v
evaluate Operator {header = Header {typeId = tId}, subPackets = ps}
  | t == 0 = sum $ fmap evaluate ps
  | t == 1 = product $ fmap evaluate ps
  | t == 2 = minimum $ fmap evaluate ps
  | t == 3 = maximum $ fmap evaluate ps
  | t == 5 =
    let v1 = (evaluate . head) ps
        v2 = (evaluate . head . tail) ps
     in if v1 > v2 then 1 else 0
  | t == 6 =
    let v1 = (evaluate . head) ps
        v2 = (evaluate . head . tail) ps
     in if v1 < v2 then 1 else 0
  | t == 7 =
    let v1 = (evaluate . head) ps
        v2 = (evaluate . head . tail) ps
     in if v1 == v2 then 1 else 0
  | otherwise = error $ "unrecognized type Id " ++ show t
  where
    t = binaryToNum tId

sixteenthDecemberSolution2 :: IO Int
sixteenthDecemberSolution2 = solution evaluate <$> input
