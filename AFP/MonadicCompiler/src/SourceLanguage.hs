module SourceLanguage where

data Prog
  = Assign Name Expr
  | If Expr Prog Prog
  | While Expr Prog
  | Seqn [Prog]

data Expr
  = Val Int
  | Var Name
  | App Op Expr Expr

type Name = Char

data Op = Add | Sub | Div | Mul
  deriving (Show, Eq)
