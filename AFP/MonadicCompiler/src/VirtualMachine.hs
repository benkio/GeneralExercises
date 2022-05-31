module VirtualMachine where

import SourceLanguage

type Stack = [Int]
type Mem = [(Name, Int)]
type Code = [Inst]
type Label = Int

data Inst = PUSH Int
          | PUSHV Name
          | POP Name
          | DO Op
          | Jump Label
          | JumpZ Label
          | LABEL Label

comp :: Prog -> Code
comp = undefined

emptyMem :: Mem
emptyMem = []

emptyStack :: Stack
emptyStack = []

exec :: Code -> Mem
exec = undefined
