import GoatAST
import SymTable

genCallTable :: Program -> CallTable
genCallTable (Program m)
  = genCall Map.empty m

genCall ::
genCall m []
  = m
genCall m x:xs
  = case x of (Proc id fp _ _) -> genCall (Map.insert id fp) xs

genProcTable :: Program -> ProcTable
genProcTable (Program m)
  = genProc Map.empty m

genProc :: ProcTable -> [Proc] -> ProcTable
genProc m []
  = m
genProc m x:xs
  = case x of (Proc id fp lv _) -> genProc (Map.insert id (genVarTable Map.empty fp lv 0) m) xs

genVarTable :: VarTable -> [Param] -> [Decl] -> Int -> VarTable
genVarTable m [] [] _
  = m
genVarTable m x:xs _ n
  = case x of (Param _ _ id) -> genVarTable (Map.insert id (paramInfo x n) m) xs (n + 1)
genVarTable m [] x:xs n
  = case x of (DeclVar _ id) -> genVarTable (Map.insert id (declInfo x n) m) xs (n + 1)
              (DeclArray _ id a) -> genVarTable (Map.insert id (declInfo x n) m) xs (n + a)
              (DeclMatrix _ id a b) -> genVarTable (Map.insert id (declInfo x n) m) xs (n + a * b)

paramInfo :: Param -> Int -> VarInfo
paramInfo (Param indic tp _) n
  = VarInfo tp n False (1, 0) indic

declInfo :: Decl -> Int -> VarInfo
declInfo (DeclVar tp _) n
  = VarInfo tp n True (0, 0) Val
declInfo (DeclArray tp _ a) n
  = VarInfo tp n True (a, 0) Val
declInfo (DeclMatrix tp _ a b) n
  = VarInfo tp n True (a, b) Val
