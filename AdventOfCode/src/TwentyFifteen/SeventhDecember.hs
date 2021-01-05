module TwentyFifteen.SeventhDecember where

import           Control.Monad.Zip
import           Data.Bits
import           Data.List         (sort, (\\))
import           Data.Map          as M (Map, empty, insert, lookup)
import           Data.Maybe        (fromJust, isJust, isNothing)
import           Data.Word         (Word16)
import           Text.Read         (readMaybe)

data Ops
  = ASSIGN String Word16
  | ASSIGNREF String String
  | RSHIFT String String Int
  | LSHIFT String String Int
  | NOT String String
  | OR String String String
  | AND1 String String
  | AND String String String
  deriving (Show, Eq, Ord)

input :: IO [Ops]
input =
  orderByReference [] . sort . fmap parseOps . lines <$>
  readFile "input/2015/7December.txt"

parseOps :: String -> Ops
parseOps s =
  let (left, rest) = break ('-' ==) s
      (_, right) = break (' ' ==) rest
   in parseOpsAssignRight (tail right) (parseOpsLeftHand left)

parseOpsLeftHand :: String -> Ops
parseOpsLeftHand [] = error "unrecognized left and operand"
parseOpsLeftHand s
  | length ws == 1 && isJust (readMaybe (head ws) :: Maybe Word16) =
    ASSIGN "" (read (head ws) :: Word16)
  | length ws == 1 && isNothing (readMaybe (head ws) :: Maybe Word16) =
    ASSIGNREF "" (head ws)
  | length ws == 2 = NOT "" (ws !! 1)
  | (ws !! 1) == "AND" && isJust (readMaybe (head ws) :: Maybe Word16) =
    AND1 "" (ws !! 2)
  | (ws !! 1) == "AND" = AND "" (head ws) (ws !! 2)
  | (ws !! 1) == "OR" = OR "" (head ws) (ws !! 2)
  | (ws !! 1) == "RSHIFT" = RSHIFT "" (head ws) (read (ws !! 2) :: Int)
  | (ws !! 1) == "LSHIFT" = LSHIFT "" (head ws) (read (ws !! 2) :: Int)
  | otherwise = error $ "unrecognized left and operand: " ++ s
  where
    ws = words s

parseOpsAssignRight :: String -> Ops -> Ops
parseOpsAssignRight opId (RSHIFT _ a b)  = RSHIFT opId a b
parseOpsAssignRight opId (LSHIFT _ a b)  = LSHIFT opId a b
parseOpsAssignRight opId (OR _ a b)      = OR opId a b
parseOpsAssignRight opId (AND _ a b)     = AND opId a b
parseOpsAssignRight opId (AND1 _ a)      = AND1 opId a
parseOpsAssignRight opId (NOT _ a)       = NOT opId a
parseOpsAssignRight opId (ASSIGNREF _ a) = ASSIGNREF opId a
parseOpsAssignRight opId (ASSIGN _ a)    = ASSIGN opId a

getDependencies :: Ops -> [String]
getDependencies (RSHIFT _ a _)  = [a]
getDependencies (LSHIFT _ a _)  = [a]
getDependencies (OR _ a b)      = [a, b]
getDependencies (AND1 _ a)      = [a]
getDependencies (AND _ a b)     = [a, b]
getDependencies (NOT _ a)       = [a]
getDependencies (ASSIGNREF _ a) = [a]
getDependencies (ASSIGN _ _)    = []

getId :: Ops -> String
getId (RSHIFT x _ _)  = x
getId (LSHIFT x _ _)  = x
getId (OR x _ _)      = x
getId (AND x _ _)     = x
getId (AND1 x _)      = x
getId (NOT x _)       = x
getId (ASSIGNREF x _) = x
getId (ASSIGN x _)    = x

orderByReference :: [Ops] -> [Ops] -> [Ops]
orderByReference xs [] = xs
orderByReference xs (y:ys) =
  if null (getDependencies y \\ fmap getId xs)
    then orderByReference (xs ++ [y]) ys
    else orderByReference xs (ys ++ [y])

opsDb :: M.Map String Word16
opsDb = M.empty

applyOps :: M.Map String Word16 -> Ops -> (M.Map String Word16, Maybe Ops)
applyOps m (ASSIGN x a) = (M.insert x a m, Nothing)
applyOps m o@(ASSIGNREF x a) =
  foldr (\v _ -> (M.insert x v m, Nothing)) (m, Just o) $ M.lookup a m
applyOps m o@(RSHIFT x a b) =
  foldr (\v _ -> (M.insert x (shift v (negate b)) m, Nothing)) (m, Just o) $
  M.lookup a m
applyOps m o@(LSHIFT x a b) =
  foldr (\v _ -> (M.insert x (shift v b) m, Nothing)) (m, Just o) $ M.lookup a m
applyOps m o@(OR x a b) =
  foldr (\(va, vb) _ -> (M.insert x (va .|. vb) m, Nothing)) (m, Just o) $
  mzip (M.lookup a m) (M.lookup b m)
applyOps m o@(AND1 x a) =
  foldr (\v _ -> (M.insert x (1 .&. v) m, Nothing)) (m, Just o) $ M.lookup a m
applyOps m o@(AND x a b) =
  foldr (\(va, vb) _ -> (M.insert x (va .&. vb) m, Nothing)) (m, Just o) $
  mzip (M.lookup a m) (M.lookup b m)
applyOps m o@(NOT x a) =
  foldr (\v _ -> (M.insert x (complement v) m, Nothing)) (m, Just o) $
  M.lookup a m

resolveOps :: M.Map String Word16 -> [Ops] -> M.Map String Word16
resolveOps m [] = m
resolveOps m (o:os) =
  let (m', o') = applyOps m o
   in foldr (\_ _ -> resolveOps m (os ++ [o])) (resolveOps m' os) o'

inputTest :: String
inputTest =
  "123 -> x\n\
\456 -> y\n\
\x AND y -> d\n\
\x OR y -> e\n\
\x LSHIFT 2 -> f\n\
\y RSHIFT 2 -> g\n\
\NOT x -> h\n\
\NOT y -> i"

solution1 :: String -> [Ops] -> Word16
solution1 oId ops = fromJust $ M.lookup oId (resolveOps opsDb ops)

testSolution1 :: Bool
testSolution1 =
  ((65079 ==) .
   solution1 "i" . orderByReference [] . sort . fmap parseOps . lines)
    inputTest

seventhDecemberSolution1 :: IO Word16
seventhDecemberSolution1 = solution1 "a" <$> input

seventhDecemberSolution2 :: IO Word16
seventhDecemberSolution2 = do
  i <- input
  a <- seventhDecemberSolution1
  let newB = ASSIGN "b" a
      input' = filter (\o -> getId o /= "b") i
  return $ solution1 "a" (input' ++ [newB])
