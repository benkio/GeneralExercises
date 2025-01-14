{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December24 where

import Data.Bifunctor (bimap, second)
import Data.Functor ((<&>))
import Data.Map (Map, fromList, insert, size, (!?))
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Debug.Trace
import Lib.Bit (fromBaseBit)
import Lib.List (find')
import Text.Printf (printf)

type Wires = Map String Int
data Op = AND | OR | XOR deriving (Show, Read, Eq)
data Gate = Gate
    { inputWires :: (String, String)
    , outputWire :: String
    , op :: Op
    }
    deriving (Eq)

data Device = Device
    { wires :: Wires
    , gates :: [Gate]
    }
    deriving (Show)

findHalfAdder :: [Gate] -> (String, String) -> Either (String,String) (String, String)
findHalfAdder gs (x, y) = maybe (Left (x,y)) (Right) $ do
    xor <- find' (\g -> op g == XOR && (inputWires g == (x, y) || inputWires g == (y, x))) gs
    and <- find' (\g -> op g == AND && (inputWires g == (x, y) || inputWires g == (y, x))) gs
    return (outputWire xor, outputWire and)

findFullAdder :: [Gate] -> (String, String) -> String -> Either (String,String) (String, String)
findFullAdder gs (x, y) c = do
    (ha1v, ha1c) <- findHalfAdder gs (x, y)
    (ha2v, ha2c) <- findHalfAdder gs (c, ha1v)
    or <- maybe (Left (ha2c, ha1c)) (Right) $ find' (\g -> op g == OR && (inputWires g == (ha2c, ha1c) || inputWires g == (ha1c, ha2c))) gs
    return (ha2v, outputWire or)

findFullNAdder :: Device -> Either (String,String) (String, String)
findFullNAdder (Device {gates = gs}) = undefined

maxZ :: Device -> Int
maxZ = maximum . (mapMaybe (fmap (\x -> read x :: Int) . ("z" `stripPrefix`) . outputWire)) . gates

test =
    input <&> \i ->
                maxZ i
                -- do
        -- (ha1v, ha1c) <- findHalfAdder (gates i) ("x00", "y00")
        -- (fa1v, fa1c) <- findFullAdder (gates i) ("x01", "y01") ha1c
        -- return (fa1v, fa1c)

runDevice :: Device -> Device
runDevice =
    until
        (null . gates)
        runDeviceSingle
  where

runDeviceSingle :: Device -> Device
runDeviceSingle (Device{wires = ws, gates = gs}) =
    Device{wires = ws', gates = gs'}
  where
    (ws', gs') = foldl evolveWires (ws, []) gs
    evolveWires :: (Wires, [Gate]) -> Gate -> (Wires, [Gate])
    evolveWires (ws, acc) g = maybe (ws, acc ++ [g]) (,acc) $ executeGate ws g

executeGate :: Wires -> Gate -> Maybe Wires
executeGate ws (Gate{inputWires = (w1, w2), outputWire = ow, op = o}) = do
    w1v <- ws !? w1
    w2v <- ws !? w2
    let ov = opToFun o w1v w2v
    return $ insert ow ov ws

extractZValue :: Wires -> Int
extractZValue ws = result
  where
    toZKey :: Wires -> Int -> Maybe Int
    toZKey m = (m !?) . printf "z%02d"
    result =
        fromBaseBit 2 . snd $ until (\(i, _) -> isNothing (toZKey ws i)) (\(i, acc) -> (i + 1, fromJust (toZKey ws i) : acc)) (0, [])

solution1 :: Device -> Int
solution1 = extractZValue . wires . runDevice

december24Solution1 :: IO Int
december24Solution1 = solution1 <$> input

solution2 :: Device -> Int
solution2 = undefined

december24Solution2 :: IO Int
december24Solution2 = solution2 <$> input

-- Transformations --------------------------------------

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

opToFun :: Op -> (Int -> Int -> Int)
opToFun AND = \x y -> boolToInt $ x == 1 && y == 1
opToFun OR = \x y -> boolToInt $ x == 1 || y == 1
opToFun XOR = \x y -> boolToInt $ x /= y

-- Instances --------------------------------------------

instance Show Gate where
    show (Gate{inputWires = (w1, w2), outputWire = ow, op = o}) =
        w1 ++ " " ++ show o ++ " " ++ w2 ++ " -> " ++ ow

-- Input ------------------------------------------------

input :: IO Device
input = parseInput <$> readFile "input/2024/December24.txt"

parseInput :: String -> Device
parseInput = (\(ws, gs) -> Device{wires = ws, gates = gs}) . bimap (parseWires) (parseGates . tail) . break (== "") . lines
  where
    parseWires :: [String] -> Wires
    parseWires = fromList . fmap parseWire
    parseWire :: String -> (String, Int) -- x00: 1
    parseWire = (\[w, v] -> (init w, read v)) . words
    parseGates :: [String] -> [Gate]
    parseGates = fmap parseGate
    parseGate :: String -> Gate -- tnw OR pbm -> gnj
    parseGate = (\[w1, op, w2, _, o] -> Gate{inputWires = (w1, w2), outputWire = o, op = read op}) . words

testInput :: Device
testInput =
    parseInput
        "x00: 1\n\
        \x01: 1\n\
        \x02: 1\n\
        \y00: 0\n\
        \y01: 1\n\
        \y02: 0\n\
        \\n\
        \x00 AND y00 -> z00\n\
        \x01 XOR y01 -> z01\n\
        \x02 OR y02 -> z02\n"

testInput' :: Device
testInput' =
    parseInput
        "x00: 1\n\
        \x01: 0\n\
        \x02: 1\n\
        \x03: 1\n\
        \x04: 0\n\
        \y00: 1\n\
        \y01: 1\n\
        \y02: 1\n\
        \y03: 1\n\
        \y04: 1\n\
        \\n\
        \ntg XOR fgs -> mjb\n\
        \y02 OR x01 -> tnw\n\
        \kwq OR kpj -> z05\n\
        \x00 OR x03 -> fst\n\
        \tgd XOR rvg -> z01\n\
        \vdt OR tnw -> bfw\n\
        \bfw AND frj -> z10\n\
        \ffh OR nrd -> bqk\n\
        \y00 AND y03 -> djm\n\
        \y03 OR y00 -> psh\n\
        \bqk OR frj -> z08\n\
        \tnw OR fst -> frj\n\
        \gnj AND tgd -> z11\n\
        \bfw XOR mjb -> z00\n\
        \x03 OR x00 -> vdt\n\
        \gnj AND wpb -> z02\n\
        \x04 AND y00 -> kjc\n\
        \djm OR pbm -> qhw\n\
        \nrd AND vdt -> hwm\n\
        \kjc AND fst -> rvg\n\
        \y04 OR y02 -> fgs\n\
        \y01 AND x02 -> pbm\n\
        \ntg OR kjc -> kwq\n\
        \psh XOR fgs -> tgd\n\
        \qhw XOR tgd -> z09\n\
        \pbm OR djm -> kpj\n\
        \x03 XOR y03 -> ffh\n\
        \x00 XOR y04 -> ntg\n\
        \bfw OR bqk -> z06\n\
        \nrd XOR fgs -> wpb\n\
        \frj XOR qhw -> z04\n\
        \bqk OR frj -> z07\n\
        \y03 OR x01 -> nrd\n\
        \hwm AND bqk -> z03\n\
        \tgd XOR rvg -> z12\n\
        \tnw OR pbm -> gnj\n"
