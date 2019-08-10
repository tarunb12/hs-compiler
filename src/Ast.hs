module Ast where

import Prelude

data Binop
  = Pow | Mul | Div -- Power, Multiply, Divide
  | Add | Sub | Mod -- Add, Subtract, Modulo
  | And | Or  | Xor -- And, Or, Xor
  | Eq  | Neq | Lte -- Equality, Inequality, Less than / Equal
  | Lt  | Gte | Gt  -- Less than, Greater than / Equal, Greater than
  | Ls  | Rs        -- Left shift, Right shift
  deriving (Eq, Show)

data Unop
  = Not | Neg
  deriving (Eq, Show)

data Primitive
  = TInt      | TReal | TComplex
  | TRational | TBool | TChar
  | TString   | TUnit | TArr
  deriving (Eq, Show)

data Literal 
  = LInt Int
  | LReal Float
  | LComplex Float Float
  | LRational Int Int
  | LBool Bool
  | LChar Char
  | LString String
  | LUnit ()
  | LArr [Expr]
  deriving (Eq, Show)

data Expr
  = Literal
  | BinOp Binop Expr Expr
  | UnOp Unop
  | Id String
  | Call String [Expr]
  | NoExpr
  deriving (Eq, Show)

data Statement
  = Block [Statement]
  | Assign String Expr
  | Expr Expr

data Program
  = Program [Statement]