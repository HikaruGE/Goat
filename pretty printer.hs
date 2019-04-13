pPrint :: Program -> String
pPrint (Program [])
  = ""
pPrint (Program [x])
  = procPrint x
pPrint (Program (x:y:ys))
  = (procPrint x) ++ "\n" ++ (pPrint (y:ys))

procPrint :: Proc -> String
procPrint (Proc a b c d)
  = (headerPrint a b) ++ (foldl indent "" (map declPrint c)) ++ "begin\n" ++ (foldl (++) "" (map (stmtPrint 4) d)) ++ "end\n"

indent :: String -> String -> String
indent a b
  = a ++ "    " ++ b

headerPrint :: Ident -> [Param] -> String
headerPrint a b
  = "proc" ++ " " ++ a ++ " " ++ "(" ++ (paramListPrint b) ++ ")\n"

paramListPrint :: [Param] -> String
paramListPrint []
  = ""
paramListPrint [x]
  = paramPrint x
paramListPrint (x:y:ys)
  = (paramPrint x) ++ ", " ++ (paramListPrint (y:ys))

paramPrint :: Param -> String
paramPrint (Param a b c)
  = (indicPrint a) ++ " " ++ (baseTypePrint b) ++ " " ++ c

indicPrint :: Indic -> String
indicPrint Val
  = "val"
indicPrint Ref
  = "ref"

baseTypePrint :: BaseType -> String
baseTypePrint BoolType
  = "bool"
baseTypePrint IntType
  = "int"
baseTypePrint FloatType
  = "float"

declPrint :: Decl -> String
declPrint (DeclVar a b)
  = (baseTypePrint a) ++ " " ++ b ++ ";\n"
declPrint (DeclArray a b c)
  = (baseTypePrint a) ++ " " ++ b ++ "[" ++ (show c) ++ "];\n"
declPrint (DeclMatrix a b c d)
  = (baseTypePrint a) ++ " " ++ b ++ "[" ++ (show c) ++ ", " ++ (show d) ++ "];\n"

stmtPrint :: Int -> Stmt -> String
stmtPrint n (Assign a b)
  = (replicate n ' ') ++ (varPrint a) ++ " := " ++ (exprPrint False b) ++ ";\n"
stmtPrint n (Read a)
  = (replicate n ' ') ++ "read " ++ (varPrint a) ++ ";\n"
stmtPrint n (Write a)
  = (replicate n ' ') ++ "write" ++ (exprPrint False a) ++ ";\n"
stmtPrint n (Call a b)
  = (replicate n ' ') ++ "call " ++ a ++ "(" ++ (exprListPrint b) ++ ");\n"
stmtPrint n (If a b)
  = (replicate n ' ') ++ "if " ++ (exprPrint False a) ++ " then\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) b)) ++ (replicate n ' ') ++ "fi\n"
stmtPrint n (IfElse a b c)
  = (replicate n ' ') ++ "if " ++ (exprPrint False a) ++ " then\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) b)) ++ (replicate n ' ') ++ "else\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) c)) ++ (replicate n ' ') ++ "fi\n"
stmtPrint n (While a b)
  = (replicate n ' ') ++ "while " ++ (exprPrint False a) ++ " do\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) b)) ++ (replicate n ' ') ++ "od\n"

varPrint :: Var -> String
varPrint (Id a)
  = a
varPrint (Array a b)
  = a ++ "[" ++ (exprPrint False b) ++ "]"
varPrint (Matrix a b c)
  = a ++ "[" ++ (exprPrint False b) ++ ", " ++ (exprPrint False c) ++ "]"

exprListPrint :: [Expr] -> String
exprListPrint []
  = ""
exprListPrint [x]
  = exprPrint False x
exprListPrint (x:y:ys)
  = (exprPrint False x) ++ ", " ++ (exprListPrint (y:ys))

exprPrint :: Bool -> Expr -> String
exprPrint _ (BoolConst True)
  = "true"
exprPrint _ (BoolConst False)
  = "false"
exprPrint _ (IntConst a)
  = show a
exprPrint _ (FloatConst a)
  = show a
exprPrint _ (StrConst a)
  = "\"" ++ a ++ "\""
exprPrint _ (Var a)
  = varPrint a
exprPrint False (Unary a b)
  = (unaPrint a) ++ (exprPrint True b)
exprPrint True (Unary a b)
  = "(" ++ (unaPrint a) ++ (exprPrint True b) ++ ")"
exprPrint _ (Binary a (Unary b c) d)
  = (exprPrint False (Unary b c)) ++ (binPrint a) ++ (exprPrint True d)
exprPrint False (Binary a b c)
  = (exprPrint True b) ++ (binPrint a) ++ (exprPrint True c)
exprPrint True (Binary a b c)
  = "(" ++ (exprPrint True b) ++ ")" ++ (binPrint a) ++ "(" ++ (exprPrint True c) ++ ")"

unaPrint :: UnaOp -> String
unaPrint Neg
  = "!"
unaPrint Minus
  = "-"

binPrint :: BinOp -> String
binPrint Add
  = " + "
binPrint Sub
  = " - "
binPrint Mul
  = " * "
binPrint Div
  = " / "
binPrint And
  = " && "
binPrint Or
  = " || "
binPrint Eq
  = " = "
binPrint NotEq
  = " != "
binPrint Lt
  = " < "
binPrint LtEq
  = " <= "
binPrint Gt
  = " > "
binPrint GtEq
  = " >= "
