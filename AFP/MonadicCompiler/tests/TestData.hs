module TestData where

import SourceLanguage
import VirtualMachine

factorialProg :: Int -> Prog
factorialProg n =
  Seqn
    [ Assign 'A' (Val 1),
      Assign 'B' (Val n),
      While
        (Var 'B')
        ( Seqn
            [ Assign 'A' (App Mul (Var 'A') (Var 'B')),
              Assign 'B' (App Sub (Var 'B') (Val 1))
            ]
        )
    ]

factorialCode :: Int -> Code
factorialCode n =
  [ PUSH 1,
    POP 'A',
    PUSH n,
    POP 'B',
    LABEL 0,
    PUSHV 'B',
    JUMPZ 1,
    PUSHV 'A',
    PUSHV 'B',
    DO Mul,
    POP 'A',
    PUSHV 'B',
    PUSH 1,
    DO Sub,
    POP 'B',
    JUMP 0,
    LABEL 1
  ]

factorialFinalMem :: Int -> Mem
factorialFinalMem n = [('A', product [1 .. n]), ('B', 0)]
