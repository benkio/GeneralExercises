module VirtualMachine where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.List (find, findIndex)
import Data.Maybe (fromMaybe)
import SourceLanguage

type Stack = [Int]

type Mem = [(Name, Int)]

type Code = [Inst]

type Label = Int

data Inst
    = PUSH Int
    | PUSHV Name
    | POP Name
    | DO Op
    | JUMP Label
    | JUMPZ Label
    | LABEL Label
    deriving (Show, Eq)

-- compilation ----------------------------------------------------------------

comp :: Prog -> Code
comp prog = execWriter $ evalStateT (comp' prog) 0

comp' :: Prog -> StateT Label (Writer Code) ()
comp' (Assign name expr) = (lift . tell) $ compExpr expr ++ [POP name]
comp' (Seqn []) = return ()
comp' (Seqn (p : ps)) = comp' p >> comp' (Seqn ps)
comp' (While expr prog) = do
    labelCount <- get
    (lift . tell) $ [LABEL labelCount] ++ compExpr expr ++ [JUMPZ (labelCount + 1)]
    put (labelCount + 2)
    comp' prog
    (lift . tell) [JUMP labelCount, LABEL (labelCount + 1)]
comp' (If expr prog1 prog2) = do
    labelCount <- get
    (lift . tell) $ compExpr expr ++ [JUMPZ labelCount]
    put (labelCount + 2)
    comp' prog1
    (lift . tell) $ [JUMP (labelCount + 1), LABEL labelCount]
    comp' prog2
    (lift . tell) [LABEL (labelCount + 1)]

compExpr :: Expr -> Code
compExpr (Val val) = [PUSH val]
compExpr (Var name) = [PUSHV name]
compExpr (App op exp1 exp2) = compExpr exp1 ++ compExpr exp2 ++ [DO op]

-- execution -----------------------------------------------------------

emptyMem :: Mem
emptyMem = []

emptyStack :: Stack
emptyStack = []

exec :: Code -> Mem
exec code =
    (\(_, m, _) -> m) $ execState (compilerStateMachine (head code) 0) (emptyStack, emptyMem, code)
  where
    compilerStateMachine :: Inst -> Int -> State (Stack, Mem, Code) ()
    compilerStateMachine i p = do
        p' <- exec' i p
        (_, _, cs) <- get
        if length cs == p'
            then return ()
            else compilerStateMachine (cs !! p') p'

exec' :: Inst -> Int -> State (Stack, Mem, Code) Int
exec' (PUSH value) p = withState (\(st, mem, cs) -> (value : st, mem, cs)) (return (p + 1))
exec' (PUSHV varName) p =
    withState
        ( \(st, mem, cs) ->
            let newStack = (maybe st (\v -> v : st) . fmap snd . find (\(n, _) -> n == varName)) mem
             in (newStack, mem, cs)
        )
        (return (p + 1))
exec' (POP varName) p =
    withState
        ( \(st, mem, cs) ->
            if null st
                then (st, mem, cs)
                else
                    let mem' = (((varName, head st) :) . filter (\(n, _) -> n /= varName)) mem
                     in (tail st, mem', cs)
        )
        (return (p + 1))
exec' (DO op) p =
    withState
        ( \(st, mem, cs) ->
            if length st < 2
                then (st, mem, cs)
                else
                    let v = execOp ((head . tail) st) (head st) op
                     in (v : (drop 2 st), mem, cs)
        )
        (return (p + 1))
exec' (LABEL _) p = return (p + 1)
exec' (JUMP label) p = do
    (_, _, cs) <- get
    return $ fromMaybe (p + 1) $ findIndex (\i -> i == (LABEL label)) cs
exec' (JUMPZ label) p = do
    (st, _, _) <- get
    if (not . null) st && head st == 0
        then exec' (JUMP label) p
        else return (p + 1)

execOp :: Int -> Int -> Op -> Int
execOp v1 v2 Add = v1 + v2
execOp v1 v2 Sub = v1 - v2
execOp v1 v2 Div = v1 `div` v2
execOp v1 v2 Mul = v1 * v2
