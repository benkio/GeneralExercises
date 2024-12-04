module TwentyTwentyFour.December03 where

import Data.Either (fromRight)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

data Exp
    = Mul Int Int
    | Do
    | Dont
    deriving (Show)

type Parser = Parsec Void String

input :: IO String
input = readFile "input/2024/December03.txt"

testInput, testInput2 :: String
testInput =
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
testInput2 =
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

parseMul :: Parser Exp
parseMul = do
    string "mul("
    v1 <- decimal
    char ','
    v2 <- decimal
    char ')'
    return $ Mul v1 v2

parseDo, parseDont, parseExpr :: Parser Exp
parseDo = string "do()" >> return Do
parseDont = string "don't()" >> return Dont
parseExpr = foldl1 (<|>) [parseMul, parseDo, parseDont]

collectParser :: Parser a -> Parser [a]
collectParser p = do
    muls <- many (try p)
    input <- getInput
    if null input
        then return muls
        else (muls ++) <$> (anySingle >> collectParser p)

solution1 :: String -> Int
solution1 s =
    fromRight 0 $
        foldl (\acc (Mul v1 v2) -> acc + (v1 * v2)) 0 <$> parse (collectParser parseMul) "" s

december03Solution1 :: IO Int
december03Solution1 = solution1 <$> input

solution2 :: String -> Int
solution2 s =
    fromRight 0 $
        snd
            <$> foldl compute (True, 0)
            <$> parse (collectParser parseExpr) "" s
  where
    compute (True, acc) (Mul v1 v2) = (True, acc + v1 * v2)
    compute (False, acc) (Mul _ _) = (False, acc)
    compute (_, acc) Do = (True, acc)
    compute (_, acc) Dont = (False, acc)

december03Solution2 :: IO Int
december03Solution2 = solution2 <$> input
