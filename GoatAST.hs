module GoatAST where

-----------------------------------
-- Specification of an AST for Goat 
-----------------------------------

type Ident = String

data GoatProgram
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
  | DeclArray BaseType Ident Integer
  | DeclMatrix BaseType Ident Integer Integer
    deriving (Show, Eq)
 
data Stmt 
  = Assign Ident Expr
  | Read Var
  | Write Expr
  | Call Ident [Expr]
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
    deriving (Show, Eq)

data Expr
  = Const Const
  | Var Var
  | Unary UnaOp Expr
  | Binary BinOp Expr Expr
    deriving (Show, Eq)
  --   BoolConst Bool
  -- | IntConst Int
  -- | FloatConst Float
  -- | StrConst String

  -- | Id Ident
  -- | Array Ident Expr
  -- | Matrix Ident Expr Expr


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

-- data Lvalue 
--   = LId Ident
--     deriving (Show, Eq)

data Var
  = Id Ident
  | Array Ident Expr
  | Matrix Ident Expr Expr
    deriving (Show, Eq)

data Const
  = BoolConst Bool
  | IntConst Integer
  | FloatConst Double
  | StrConst String
    deriving (Show, Eq)