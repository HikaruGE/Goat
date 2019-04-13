module GoatAST where

-----------------------------------
-- Specification of an AST for Goat dsadsasdad
-----------------------------------

type Ident = String

data Program
  = Program [Proc]
    deriving (Show, Eq)

data Proc
  = Proc Ident [Param] [Decl] [Stmt]
    deriving (Show, Eq)

data Param
  = Param Indic BaseType Ident
    deriving (Show, Eq)

data Decl
  = DeclVar BaseType Ident
  | DeclArray BaseType Ident Int
  | DeclMatrix BaseType Ident Int Int
    deriving (Show, Eq)

data Stmt
  = Assign Var Expr
  | Read Var
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
  | Var Var
  | Unary UnaOp Expr
  | Binary BinOp Expr Expr
    deriving (Show, Eq)

data BinOp
  = Add | Sub | Mul | Div
  | And | Or
  | Eq | NotEq
  | Lt | LtEq
  | Gt | GtEq
    deriving (Show, Eq)

data UnaOp
  = Neg
  | Minus
    deriving (Show, Eq)

data Indic
  = Val | Ref
    deriving (Show, Eq)

data BaseType
  = BoolType | IntType | FloatType
    deriving (Show, Eq)

data Var
  = Id Ident
  | Array Ident Expr
  | Matrix Ident Expr Expr
    deriving (Show, Eq)
