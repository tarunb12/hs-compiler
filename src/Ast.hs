module Ast where

data BinOp
  = Pow | Mul | Div -- Power, Multiply, Divide
  | Add | Sub | Mod -- Add, Subtract, Modulo
  | And | Or  | Xor -- And, Or, Xor
  | Eq  | Neq | Lte -- Equality, Inequality, Less than / Equal
  | Lt  | Gte | Gt  -- Less than, Greater than / Equal, Greater than
  | Ls  | Rs        -- Left shift, Right shift

data UnOp
  = Not | Neg

data Datatype
  = TInt | TReal | TComplex
  | TRational | TBool | TChar
  | TString | TUnit | TArr

data Expr
  = LInt Int
  | LReal Float
  | LComplex Float Float
  | LRational Int Int
  | LBool Bool
  | LChar Char
  | LString String
  | LUnit Unit
  | LArr Expr List
