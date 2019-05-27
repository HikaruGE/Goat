import GoatAST
import SymTable
import Analyze

programCode :: Program -> ProcTable -> String
programCode (Program m) t
  = "    call proc_main\n    halt\n" ++ (procsCode m t)

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
  = (procLabel id) ++ (prolog n) ++ (paramsCode x t 0) ++ (declsCode y t m) ++ (stmtsCode z t) ++ (epilog n)
    where n = getSize t
          m = length x

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

stmtCode :: Stmt -> VarTable -> String
stmtCode (Write (Const (BoolConst True))) _
  = "    int_const r0, 1\n    call_builtin print_bool\n"
stmtCode (Write (Const (BoolConst False))) _
  = "    int_const r0, 0\n    call_builtin print_bool\n"
stmtCode (Write (Const (IntConst n))) _
  = "    int_const r0, " ++ (show n) ++ "\n    call_builtin print_int\n"
stmtCode (Write (Const (FloatConst n))) _
  = "    real_const r0, " ++ (show n) ++ "\n    call_builtin print_real\n"
stmtCode (Write (Cons t (StrConst s))) _
  = "    string_const r0, \"" ++ s ++ "\"\n    call_builtin print_string\n"
stmtCode (Write (Var (Id id))) t
  = --call the function to generate expression code

stmtCode (Call id x)
  = -- call the function to generate expression code and store the results in register r0, r1...
    -- the expression generator needs to consider whether val or ref, then load or load_address
    -- ++ "    call proc_" ++ id ++ "\n"
