module CodeGen where

import GoatAST
import SymTable
-- import Analyze

programCode :: Program -> ProcTable -> String
programCode (Program m) t
  = "    call proc_main\n" ++
    "    halt\n" ++ 
    (procsCode m t)

procsCode :: [Proc] -> ProcTable -> String
procsCode [] _
    = ""
procsCode (x:xs) t
    = (procCode x t) ++ (procsCode xs t)

procCode :: Proc -> ProcTable -> String
procCode (Proc id x y z) t
    =   (procLabel id) ++
        "    push_stack_frame 1\n" ++
        "#decl\n" ++
        (stmtsCode z t) ++
        "    pop_stack_frame 1\n" ++
        "    return\n"

-- procCode (Proc id x y z) t
--     = (procLabel id) ++ (prolog n) ++ 
--     (paramsCode x t 0) ++ (declsCode y t m) ++ (stmtsCode z t) ++ (epilog n)
--     where n = getSize id t
--           m = getFPSize id t

prolog :: Int -> String
prolog n = "    push_stack_frame " ++ (show n) ++ "\n"

epilog :: Int -> String
epilog n = "    pop_stack_frame " ++ (show n) ++ 
           "\n    return\n"
          
paramsCode :: [Param] -> ProcTable -> Int -> String
paramsCode [] _ _
    = ""
paramsCode (x:xs) t n
    = (paramCode x t n) ++ (paramsCode xs t (n + 1))

paramCode :: Param -> ProcTable -> Int -> String
paramCode _ _ n
    = "    store " ++ s ++ ", r" ++ s ++ "\n"
    where s = show n

blockLabel :: String -> String
blockLabel s
    = "label_" ++ s ++ ":\n"

procLabel :: String -> String
procLabel s
    = "proc_" ++ s ++ ":\n"

stmtsCode :: [Stmt] -> ProcTable -> String
stmtsCode [] _
    = ""
stmtsCode (x:xs) t
    = (stmtCode x t) ++ (stmtsCode xs t)

stmtCode :: Stmt -> ProcTable -> String
stmtCode (Write expr) t
    = "    " ++ (writeOpr expr t) ++" r0, " ++ (exprCode expr t) ++ "\n" ++
      "    call_builtin " ++ (writeBuiltin expr t) ++ "\n"

exprCode :: Expr -> ProcTable -> String
exprCode (BoolConst True) _ = "\"true\""
exprCode (BoolConst False) _ = "\"false\""
exprCode (IntConst i) _ = show(i)
exprCode (FloatConst f) _ = show(f)
exprCode (StrConst s) _ = "\"" ++ s ++ "\""

writeOpr :: Expr -> ProcTable -> String
writeOpr (BoolConst b) _ = "string_const"
writeOpr (IntConst b) _ = "int_const"
writeOpr (FloatConst b) _ = "real_const"
writeOpr (StrConst b) _ = "string_const"

writeBuiltin :: Expr -> ProcTable -> String
writeBuiltin (BoolConst b) _ = "print_string"
writeBuiltin (IntConst b) _ = "print_int"
writeBuiltin (FloatConst b) _ = "print_real"
writeBuiltin (StrConst b) _ = "print_string"
