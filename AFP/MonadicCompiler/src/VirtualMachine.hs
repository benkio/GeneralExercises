module VirtualMachine where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
import Data.Foldable (foldl')
import Data.List (find)
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
  snd $ execState (foldl' (\s i -> s >> exec' i) (return ()) code) (emptyStack, emptyMem)

exec' :: Inst -> State (Stack, Mem) ()
exec' (PUSH value) = withState (\(st, mem) -> (value : st, mem)) (return ())
exec' (PUSHV varName) =
  withState
    ( \(st, mem) ->
        let newStack = (maybe st (\v -> v : st) . fmap snd . find (\(n, _) -> n == varName)) mem
         in (newStack, mem)
    )
    (return ())
exec' (POP varName) = undefined
exec' (DO op) = undefined
exec' (JUMP label) = undefined
exec' (JUMPZ label) = undefined
exec' (LABEL label) = undefined
