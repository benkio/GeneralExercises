{-# LANGUAGE TupleSections #-}

module TwentyTwentyFour.December24 where

import Data.Bifunctor (bimap, second)
import Data.Either (fromLeft)
import Data.Functor ((<&>))
import Data.List (intercalate, maximum, sort, stripPrefix)
import Data.Map (Map, fromList, insert, size, (!?))
import Data.Maybe (fromJust, fromMaybe, isNothing, mapMaybe)
import GHC.Data.Maybe (expectJust, firstJusts)
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

findHalfAdder :: [Gate] -> (String, String) -> Either (String, String) (String, String)
findHalfAdder gs (x, y) = do
    xor <- maybe (Left (findErrorGate gs (x, y) XOR)) Right $ searchGate (x, y) XOR gs
    and <- maybe (Left (findErrorGate gs (x, y) AND)) Right $ searchGate (x, y) AND gs
    return (outputWire xor, outputWire and)

findFullAdder :: [Gate] -> (String, String) -> String -> Either (String, String) (String, String)
findFullAdder gs (x, y) c = do
    (ha1v, ha1c) <- findHalfAdder gs (x, y)
    (ha2v, ha2c) <- findHalfAdder gs (c, ha1v)
    or <- maybe (Left (findErrorGate gs (ha2c, ha1c) OR)) Right $ searchGate (ha2c, ha1c) OR gs
    return (ha2v, outputWire or)

searchGate :: (String, String) -> Op -> [Gate] -> Maybe Gate
searchGate (x, y) o = find' (\g -> op g == o && (inputWires g == (x, y) || inputWires g == (y, x)))

findErrorGate :: [Gate] -> (String, String) -> Op -> (String, String)
findErrorGate gs (x, y) o =
    gateToFixedWiring (x, y)
        . expectJust (printf "[findErrorGate]: can't find a gate to fix %s - %s" (show (x, y)) (show o))
        . firstJusts
        $ [searchGateSingle x o gs, searchGateSingle y o gs]
  where
    searchGateSingle :: String -> Op -> [Gate] -> Maybe Gate
    searchGateSingle i o = find' (\g -> op g == o && ((fst . inputWires) g == i || (snd . inputWires) g == i))
    gateToFixedWiring :: (String, String) -> Gate -> (String, String)
    gateToFixedWiring (x, y) (Gate{inputWires = (x', y')})
        | x == x' = (y, y')
        | x == y' = (x', y)
        | y == x' = (x, y')
        | y == y' = (x, x')

findFullNAdder :: Device -> Either (String, String) (String, String)
findFullNAdder d@(Device{gates = gs}) = fst $ foldl foldAdder (findHalfAdder gs ("x00", "y00"), 0) adder
  where
    mz = maxZ d
    adder = replicate (mz - 1) (findFullAdder gs)
    foldAdder ::
        (Either (String, String) (String, String), Int) ->
        ((String, String) -> String -> Either (String, String) (String, String)) ->
        (Either (String, String) (String, String), Int)
    foldAdder (acc, i) fa =
        let
            xN = printf "x%02d" (i + 1)
            yN = printf "y%02d" (i + 1)
         in
            (acc >>= \(_, c) -> fa (xN, yN) c, i + 1)

replaceOutputWire :: Gate -> String -> String -> Gate
replaceOutputWire g@(Gate{outputWire = ow}) start end
    | ow == start = g{outputWire = end}
    | ow == end = g{outputWire = start}
    | otherwise = g{outputWire = ow}

swapWires :: Device -> (String, String) -> Device
swapWires d@(Device{gates = gs}) (x, y) =
    d{gates = foldl fixGates [] gs}
  where
    fixGates acc g =
        acc ++ [replaceOutputWire g x y]
    xGates = filter (\g -> (fst . inputWires) g == x || (snd . inputWires) g == x || outputWire g == x) gs
    yGates = filter (\g -> (fst . inputWires) g == y || (snd . inputWires) g == y || outputWire g == y) gs

fixDevice :: Device -> ([String], Device)
fixDevice = go 4
  where
    go 0 d = ([], d)
    go n d =
        let (start, end) = fromLeft (error "expected Left got Right") $ findFullNAdder d
            (swaps, d') = go (n - 1) $ swapWires d (start, end)
         in (swaps ++ [start, end], d')

maxZ :: Device -> Int
maxZ = maximum . mapMaybe (fmap (\x -> read x :: Int) . ("z" `stripPrefix`) . outputWire) . gates

runDevice :: Device -> Device
runDevice =
    until
        (null . gates)
        runDeviceSingle

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

solution2 :: Device -> String
solution2 = intercalate "," . sort . fst . fixDevice

december24Solution2 :: IO String
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
parseInput = (\(ws, gs) -> Device{wires = ws, gates = gs}) . bimap parseWires (parseGates . tail) . break (== "") . lines
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
