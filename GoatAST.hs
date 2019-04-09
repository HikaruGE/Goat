module GoatAST where

-----------------------------------
-- Specification of an AST for Goat 
-----------------------------------

type Ident = String

data Program
  = Program [Proc]

data Proc
  = Proc Ident [Param] [Decl] [Stmt]

data Param
  = Param Indic BaseType Ident

data Decl
  = Decl BaseType Ident

data Stmt 
  = Assign Lvalue Expr
  | Read Lvalue
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

data Expr
  = BoolConst Bool
  | IntConst Int
  | FloatConst Float
  | StrConst String
  | Id Ident
  | Unary UnaOp Exp
  | Binary BinOp Exp Exp

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Eq
  | NotEq
  | Lt
  | LtEq
  | Gt
  | GtEq
  deriving (Show, Eq)

data UnaOp
  = Neg
  | Minus
  deriving (Show, Eq)

data Indic
  = Val | Ref

data BaseType 
  = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Lvalue 
  = LId Ident
    deriving (Show, Eq)
