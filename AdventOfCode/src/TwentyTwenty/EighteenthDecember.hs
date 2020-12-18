-------------------------------------------------------------------------------
--                           Advent Of Code - day 18                          --
-------------------------------------------------------------------------------
module TwentyTwenty.EighteenthDecember where

import Data.Bifunctor (bimap, first, second)
import Data.List (drop, stripPrefix)
import Data.Maybe (fromJust, isJust)
import Text.Read (readMaybe)

data Exp
  = Plus Exp Exp
  | Prod Exp Exp
  | Paren Exp
  | Value Int
  deriving (Show)

input :: IO String
input = readFile "input/2020/18December.txt"

interpreter :: Exp -> Int
interpreter (Plus e1 e2) = interpreter e1 + interpreter e2
interpreter (Prod e1 e2) = interpreter e1 * interpreter e2
interpreter (Paren e) = interpreter e
interpreter (Value x) = x

stripParen :: (String, String) -> Int -> (String, String)
stripParen x 0 = x
stripParen (v, '(':xs) count = stripParen (v ++ "(", xs) (count + 1)
stripParen (v, ')':xs) count = stripParen (v ++ ")", xs) (count - 1)
stripParen (v, x:xs) count = stripParen (v ++ [x], xs) count

invertParent :: Char -> Char
invertParent '(' = ')'
invertParent ')' = '('
invertParent x = x

reverseWithParent :: String -> String
reverseWithParent = fmap invertParent . reverse

parseValue :: (String, Maybe Exp) -> (String, Maybe Exp)
parseValue ([], x) = ([], x)
parseValue (s@(x:xs), e)
  | (isJust . (\y -> readMaybe [y] :: Maybe Int)) x =
    (xs, Value <$> (readMaybe [x] :: Maybe Int))
  | otherwise = (s, e)

parseParen ::
     (String -> (String, Maybe Exp))
  -> (String, Maybe Exp)
  -> (String, Maybe Exp)
parseParen _ ([], x) = ([], x)
parseParen expParser (s@(x:xs), e)
  | x == '(' =
    let (parenContent', rest) = stripParen ("", xs) 1
        parenContent = init parenContent'
     in if null rest
          then expParser parenContent
          else first (rest ++) $ expParser parenContent
  | otherwise = (s, e)

parsePlus ::
     (String -> (String, Maybe Exp))
  -> (String, Maybe Exp)
  -> (String, Maybe Exp)
parsePlus expParser ('+':xs, parsedExp) =
  second (\mp -> Plus <$> parsedExp <*> mp) $ expParser xs
parsePlus _ s = s

parseProd ::
     (String -> (String, Maybe Exp))
  -> (String, Maybe Exp)
  -> (String, Maybe Exp)
parseProd expParser ('*':xs, parsedExp) =
  second (\mp -> Prod <$> parsedExp <*> mp) $ expParser xs
parseProd _ s = s

parseExpStep :: (String, Maybe Exp) -> (String, Maybe Exp)
parseExpStep x =
  foldl
    (\acc p -> p acc)
    x
    [parseValue, parseParen parseExp, parsePlus parseExp, parseProd parseExp]

parseExp :: String -> (String, Maybe Exp)
parseExp s = until (null . fst) parseExpStep (s, Nothing)

computeExp :: Bool -> String -> Int
computeExp groupPlus =
  interpreter .
  fromJust .
  snd .
  parseExp .
  (\x ->
     if groupPlus
       then selectPlusFactors x 0
       else x) .
  reverseWithParent . filter (' ' /=)

tests :: [Bool]
tests =
  (uncurry (==) . second (computeExp False)) <$>
  [ (71, "1 + 2 * 3 + 4 * 5 + 6")
  , (51, "1 + (2 * 3) + (4 * (5 + 6))")
  , (26, "2 * 3 + (4 * 5)")
  , (437, "5 + (8 * 3 + 9 + 3 * 4 * 3)")
  , (12240, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
  , (13632, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
  ]

eighteenthDecemberSolution1 :: IO Int
eighteenthDecemberSolution1 = sum . fmap (computeExp False) . lines <$> input

tests2 :: [Bool]
tests2 =
  (uncurry (==) . second (computeExp True)) <$>
  [ (231, "1 + 2 * 3 + 4 * 5 + 6")
  , (51, "1 + (2 * 3) + (4 * (5 + 6))")
  , (46, "2 * 3 + (4 * 5)")
  , (1445, "5 + (8 * 3 + 9 + 3 * 4 * 3)")
  , (669060, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))")
  , (23340, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
  ]

selectPlusFactors :: String -> Int -> String
selectPlusFactors s i
  | '+' `elem` drop i s =
    let (prev, plusNext) = first (take i s ++) $ span ('+' /=) (drop i s)
        (prevLeftFactor, leftFact) = findFactor prev False
        (afterRightFactor, rightFact) = findFactor (tail plusNext) True
        charToDrop = ((+ 2) . length) (prevLeftFactor ++ leftFact)
        newExp =
          prevLeftFactor ++
          "(" ++ leftFact ++ "+" ++ rightFact ++ ")" ++ afterRightFactor
     in selectPlusFactors newExp charToDrop
  | otherwise = s

findFactor :: String -> Bool -> (String, String)
findFactor x True =
  let (after, _) = valueOrParen (x, Nothing)
   in (after, (reverse . fromJust . stripPrefix (reverse after) . reverse) x)
findFactor x False =
  let s' = reverseWithParent x
      (prev, _) = valueOrParen (s', Nothing)
   in bimap
        reverseWithParent
        (fromJust . stripPrefix (reverseWithParent prev) . reverseWithParent)
        (prev, s')

valueOrParen :: (String, Maybe Exp) -> (String, Maybe Exp)
valueOrParen x = foldl (\acc p -> p acc) x [parseValue, parseParen parseExp]

eighteenthDecemberSolution2 :: IO Int
eighteenthDecemberSolution2 = sum . fmap (computeExp True) . lines <$> input
