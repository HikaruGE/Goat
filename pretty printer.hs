pPrint :: Program -> String
pPrint (Program [])
  = ""
pPrint (Program (x:xs))
  = (procPrint x) ++ "\n" ++ (pPrint xs)

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
  = (replicate n " ") ++ (varPrint a) ++ " := " ++ (exprPrint b) ++ ";\n"
stmtPrint n (Read a)
  = (replicate n " ") ++ "read " ++ (varPrint a) ++ ";\n"
stmtPrint n (Write a)
  = (replicate n " ") ++ "write" ++ (exprPrint a) ++ ";\n"
stmtPrint n (Call a b)
  = (replicate n " ") ++ "call " ++ a ++ "(" ++ (exprListPrint b) ++ ");\n"
stmtPrint n (If a b)
  = (replicate n " ") ++ "if " ++ (exprPrint a) ++ " then\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) b)) ++ (replicate n " ") ++ "fi\n"
stmtPrint n (IfElse a b c)
  = (replicate n " ") ++ "if " ++ (exprPrint a) ++ " then\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) b)) ++ (replicate n " ") ++ "else\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) c)) ++ (replicate n " ") ++ "fi\n"
stmtPrint n (While a b)
  = (replicate n " ") ++ "while " ++ (exprPrint a) ++ " do\n" ++ (foldl (++) "" (map (stmtPrint (n + 4)) b)) ++ (replicate n " ") ++ "od\n"

varPrint :: Var -> String
varPrint (Id a)
  = a
varPrint (Array a b)
  = a ++ "[" ++ (exprPrint b) ++ "]"
varPrint (Matrix a b c)
  = a ++ "[" ++ (exprPrint b) ++ ", " ++ (exprPrint c) ++ "]"

exprListPrint :: [Expr] -> String
exprListPrint []
  = ""
exprListPrint [x]
  = exprPrint x
exprListPrint (x:y:ys)
  = (exprPrint x) ++ ", " ++ (exprListPrint (y:ys))

exprPrint :: Expr -> String
exprPrint (BoolConst a)
  = show a
exprPrint (IntConst a)
  = show a
exprPrint (FloatConst a)
  = show a
exprPrint (StrConst a)
  = show a
exprPrint (Var a)
  = varPrint a
exprPrint (Unary a b)
  = unaPrint a b
exprPrint (Binary a b c)
  = binPrint a b c

unaPrint :: UnaOp -> Expr -> String
unaPrint Neg a
  = "!" ++ (exprPrint a)
unaPrint Minus a
  = "-" ++ (exprPrint a)

binPrint :: BinOp -> Expr -> Expr -> String
binPrint Add a b
  = "(" ++ (exprPrint a) ++ " + " ++ (exprPrint b) ++ ")"
binPrint Sub a b
  = "(" ++ (exprPrint a) ++ " - " ++ (exprPrint b) ++ ")"
binPrint Mul a b
  = "(" ++ (exprPrint a) ++ " * " ++ (exprPrint b) ++ ")"
binPrint Div a b
  = "(" ++ (exprPrint a) ++ " / " ++ (exprPrint b) ++ ")"
binPrint And a b
  = "(" ++ (exprPrint a) ++ " && " ++ (exprPrint b) ++ ")"
binPrint Or a b
  = "(" ++ (exprPrint a) ++ " || " ++ (exprPrint b) ++ ")"
binPrint Eq a b
  = "(" ++ (exprPrint a) ++ " = " ++ (exprPrint b) ++ ")"
binPrint NotEq a b
  = "(" ++ (exprPrint a) ++ " != " ++ (exprPrint b) ++ ")"
binPrint Lt a b
  = "(" ++ (exprPrint a) ++ " < " ++ (exprPrint b) ++ ")"
binPrint LtEq a b
  = "(" ++ (exprPrint a) ++ " <= " ++ (exprPrint b) ++ ")"
binPrint Gt a b
  = "(" ++ (exprPrint a) ++ " > " ++ (exprPrint b) ++ ")"
binPrint GtEq a b
  = "(" ++ (exprPrint a) ++ " >= " ++ (exprPrint b) ++ ")"
