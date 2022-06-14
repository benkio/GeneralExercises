module VirtualMachine where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Writer.Lazy
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

-- comp' (If expr prog1 prog2) =
--   -- let (prog1Code, labelCount1) = comp' (labelCount + 2) prog1
--   --     (prog2Code, labelCount2) = comp' labelCount1 prog2
--   --  in ( compExpr expr
--   --         ++ [JUMPZ labelCount]
--   --         ++ prog1Code
--   --         ++ [JUMP (labelCount + 1), LABEL labelCount]
--   --         ++ prog2Code
--   --         ++ [LABEL (labelCount + 1)],
--   --       labelCount2
--   --     )
-- comp' labelCount (While expr prog) =
--   let (innerWhileCode, innerLabelCount) = comp' (labelCount + 2) prog
--    in ( [LABEL labelCount]
--           ++ compExpr expr
--           ++ [JUMPZ (labelCount + 1)]
--           ++ innerWhileCode
--           ++ [JUMP labelCount, LABEL (labelCount + 1)],
--         innerLabelCount
--       )

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
exec = undefined
