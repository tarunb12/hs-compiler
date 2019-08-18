module Ast where

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
  = TInt    | TBool | TChar
  | TString | TUnit | TArr
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
  = Literal Literal
  | BinOp Binop Expr Expr
  | UnOp Unop
  | Id String
  | Call String [Expr]
  | Parens Expr
  | NoExpr
  deriving (Eq, Show)

data Statement
  = Block [Statement]
  | Expr Expr
  | Return Expr
  | Assign String Expr
  | VarDec Primitive String
  | VarDef Primitive String Expr
  | FuncDef Primitive String [Statement] [Statement]
  | If Expr Statement Statement
  | For Statement Expr Statement
  | While Expr Statement
  | Break
  | Continue
  deriving (Eq, Show)

-- List of (functions or global defs)
data Program
  = Program [Statement]
  deriving (Eq, Show)