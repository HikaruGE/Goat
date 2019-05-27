module CodeGen where

import GoatAST
import SymTable
import Analyze

import qualified Data.Map as Map

type Reg = Int

data BinOpClass 
    = Arithmetic | Comparision | Logic

compile :: Program -> String
compile ast = 
    let t = initTables in
        programCode ast t

programCode :: Program -> SynTables -> String
programCode (Program m) t
  = "    call proc_main\n" ++
    "    halt\n" ++ 
    (procsCode m t)

blockLabel :: String -> String
blockLabel s
  = "label_" ++ s ++ ":\n"

procLabel :: String -> String
procLabel s
  = "proc_" ++ s ++ ":\n"

procsCode :: [Proc] -> ProcTable -> String
procsCode [] _
  = ""
procsCode x:xs t
  = case x of (Proc id _ _ _) -> (procCode x (getVarTable id t)) ++ (procsCode xs t)

procCode :: Proc -> VarTable -> String
procCode (Proc id x y z) t
    =   (procLabel id) ++
        "    push_stack_frame 1\n" ++
        "#decl\n" ++
        (stmtsCode z t) ++
        "    pop_stack_frame 1\n" ++
        "    return\n"

-- procCode :: Proc -> VarTable -> String
-- procCode (Proc id x y z) t
--   = (procLabel id) ++ (prolog n) ++ (paramsCode x t 0) ++ (declsCode y t m) ++ (stmtsCode z t) ++ (epilog n)
--     where n = getSize t
--           m = length x

prolog :: Int -> String
prolog n
  = "    push_stack_frame " ++ (show n) ++ "\n"

epilog :: Int -> String
epilog n
  = "    pop_stack_frame " ++ (show n) ++ "\n    return\n"

-- Code generator for a list of formal parameters
paramsCode :: [Param] -> VarTable -> Int -> String
paramsCode [] _ _
  = ""
paramsCode x:xs t n
  = (paramCode x t n) ++ (paramsCode xs t (n + 1))

paramCode :: Param -> VarTable -> Int -> String
paramCode _ _ n
  = "    store " ++ s ++ ", r" ++ s ++ "\n"
    where s = show n

-- Code generator for a list of declarations
declsCode :: [Decl] -> VarTable -> Int -> String
declsCode [] _ _
  = ""
declsCode x:xs t n
  = case x of (DeclVar _ _) -> (declCode x t n) ++ (declsCode xs t (n + 1))
              (DeclArray _ _ a) -> (declCode x t n) ++ (declsCode xs t (n + a))
              (DeclMatrix _ _ a b) -> (declCode x t n) ++ (declsCode xs t (n + a * b))

declCode :: Decl -> VarTable -> Int -> String
declCode x t n
  = case x of (DeclVar _ _) -> declVar x t n
              (DeclArray _ _ a) -> declArray x t n a
              (DeclMatrix _ _ a b) -> declMatrix x t n (a * b)

declVar :: Decl -> VarTable -> Int -> String
declVar (DeclVar IntType _) _ n
  = "    int_const r0, 0\n    store " ++ (show n) ++ ", r0\n"
declVar (DeclVar FloatType _) _ n
  = "    real_const r0, 0.0\n    store " ++ (show n) ++ ", r0\n"
declVar (DeclVar BoolType _) _ n
  = "    int_const r0, 0\n    store " ++ (show n) ++ ", r0\n"

declArray :: Decl -> VarTable -> Int -> Int -> String
declArray _ _ _ 0
  = ""
declArray x@(DeclArray IntType _ _) t n m
  = "    int_const r0, 0\n    store " ++ (show n) ++ ", r0\n" ++ declArray x t (n + 1) (m - 1)
declArray x@(DeclArray FloatType _ _) t n m
  = "    real_const r0, 0.0\n    store " ++ (show n) ++ ", r0\n" ++ declArray x t (n + 1) (m - 1)
declArray x@(DeclArray BoolType _ _) t n m
  = "    int_const r0, 0\n    store " ++ (show n) ++ ", r0\n" ++ declArray x t (n + 1) (m - 1)

declMatrix :: Decl -> VarTable -> Int -> Int -> String
declMatrix _ _ _ 0
  = ""
declMatrix x@(DeclMatrix IntType _ _) t n m
  = "    int_const r0, 0\n    store " ++ (show n) ++ ", r0\n" ++ declMatrix x t (n + 1) (m - 1)
declMatrix x@(DeclMatrix FloatType _ _) t n m
  = "    real_const r0, 0.0\n    store " ++ (show n) ++ ", r0\n" ++ declMatrix x t (n + 1) (m - 1)
declMatrix x@(DeclMatrix BoolType _ _) t n m
  = "    int_const r0, 0\n    store " ++ (show n) ++ ", r0\n" ++ declMatrix x t (n + 1) (m - 1)

-- Code generator for a list of statements
stmtsCode :: [Stmt] -> VarTable -> String
stmtsCode [] _
  = ""
stmtsCode x:xs t
  = (stmtCode x t) ++ (stmtsCode xs t)


-- stmtCode (Call id x)
--   = -- call the function to generate expression code and store the results in register r0, r1...
    -- the expression generator needs to consider whether val or ref, then load or load_address
    -- ++ "    call proc_" ++ id ++ "\n"
    


epilog :: Int -> String
epilog n = "    pop_stack_frame " ++ (show n) ++ 
           "\n    return\n"

paramCode :: Param -> SynTables -> Int -> String
paramCode _ _ n
    = "    store " ++ s ++ ", r" ++ s ++ "\n"
    where s = show n

stmtsCode :: [Stmt] -> SynTables -> String
stmtsCode [] _
    = ""
stmtsCode (x:xs) t
    = (stmtCode x t) ++ (stmtsCode xs t)

stmtCode :: Stmt -> SynTables -> String
stmtCode (Write expr) t = 
    let 
        (code, reg, ty) = exprCode expr 0
    in
        code ++ 
        "    call_builtin " ++ (writeBuiltin ty) ++ "\n"

        
exprCode :: Expr -> Reg -> (String, Reg, BaseType)
exprCode (BoolConst b) r = ("    int_const" ++ regToStr(r) ++ ", "++ show(val) ++"\n",
                                r,
                                BoolType)
                            where 
                                val = boolToInt b
exprCode (IntConst i) r = ("    int_const" ++ regToStr(r) ++ ", "++ show(i) ++"\n",
                                r,
                                IntType)
exprCode (FloatConst f) r = ("    real_const" ++ regToStr(r) ++ ", "++ show(f) ++"\n",
                                r,
                                FloatType)
exprCode (StrConst s) r = ("    string_const" ++ regToStr(r) ++ ", \""++ s ++"\"\n",
                                r,
                                StrType)

exprCode (Unary unaOp expr) r = 
    let
        (code, reg, ty) = exprCode expr r
        code' = 
            case unaOp of
                Neg -> "    not" ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
                Minus -> "    neg_" ++ (minusCodeByTy ty) ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"
    in
        (code++code', reg, ty)

exprCode (Binary binOp lexpr rexpr) r =
    let
        left@(lcode, lreg, lty) = exprCode lexpr r
        right@(rcode, rreg, rty) = exprCode rexpr (r+1)
        convertCode = tyConvert binOp left right
        code' = "\n"
    in
        (lcode ++ rcode ++ code', lreg, lty)

minusCodeByTy :: BaseType -> String
minusCodeByTy IntType = "int"
minusCodeByTy FloatType = "real"

boolToInt :: Bool -> Int
boolToInt True = 1 
boolToInt False = 0

writeOpr :: Expr -> ProcTable -> String
writeOpr (BoolConst b) _ = "string_const"
writeOpr (IntConst b) _ = "int_const"
writeOpr (FloatConst b) _ = "real_const"
writeOpr (StrConst b) _ = "string_const"

writeBuiltin :: BaseType -> String 
writeBuiltin ty = case ty of
                BoolType -> "print_bool"
                IntType -> "print_int"
                FloatType -> "print_real"
                StrType -> "print_string"

regToStr :: Int -> String
regToStr i = " r" ++ show(i)

-- isRegNeedInc :: Expr -> Expr -> (Bool, Expr, Expr)
-- isRegNeedInc expr expr =

-- isBinExpr :: Expr -> Bool
-- isBinExpr (Binary _ _ _) = True
-- isBinExpr _ = False

tyConvert :: BinOp -> (String, Reg, BaseType) -> (String, Reg, BaseType) -> String
tyConvert binOp (_,reg1,ty1) (_,reg2,ty2) =
    case (classifyBinOp binOp) of
        Arithmetic -> 
            if ty1 == ty2 then
                ""
            else
                let 
                    reg = convertReg (reg1,ty1) (reg2,ty2) 
                in
                    "    int_to_real" ++ regToStr(reg) ++ "," ++ regToStr(reg) ++ "\n"

        Comparision -> 
            ""
        Logic ->
            "" 

convertReg :: (Reg, BaseType) -> (Reg, BaseType) -> Reg
convertReg (reg1,ty1) (reg2,ty2) = 1
-- convertReg (reg1,ty1) (reg2,ty2) =
--     if ty1 == IntType 
--         then reg1
--         else if ty2 == IntType 
--             then reg2

classifyBinOp :: BinOp -> BinOpClass
classifyBinOp binOp = 
    case binOp of
        Add -> Arithmetic
        Sub -> Arithmetic
        Mul -> Arithmetic
        Div -> Arithmetic
        Eq -> Arithmetic
        NotEq -> Comparision
        Lt -> Comparision
        LtEq -> Comparision
        Gt -> Comparision
        GtEq -> Comparision
        And -> Logic
        Or -> Logic
