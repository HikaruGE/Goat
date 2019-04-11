module Main where

import GoatAST
import Data.Char
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q
import System.Environment
import System.Exit

type Parser a
    = Parsec String Int a

lexer :: Q.TokenParser Int
lexer
    = Q.makeTokenParser
        (emptyDef
        { Q.commentLine     = "#"
        , Q.nestedComments  = True
        , Q.identStart      = letter
        , Q.identLetter     = alphaNum <|> oneOf "_'"
        , Q.opStart         = oneOf "+-*/:><=!|&"
        , Q.opLetter        = oneOf "|&="
        , Q.reservedNames   = myReserved
        , Q.reservedOpNames = myOpnames
        })

whiteSpace = Q.whiteSpace lexer
lexeme     = Q.lexeme lexer
integer    = Q.integer lexer
float      = Q.float lexer
identifier = Q.identifier lexer
colon      = Q.colon lexer
semi       = Q.semi lexer
comma      = Q.comma lexer
parens     = Q.parens lexer
squares    = Q.squares lexer
reserved   = Q.reserved lexer
reservedOp = Q.reservedOp lexer

myReserved, myOpnames :: [String]

myReserved
    = ["begin", "bool", "call", "do", "else", "end", "false"
    ,"fi", "float", "if", "int", "od", "proc", "read", "ref"
    , "then", "true", "val", "while", "write"]
myOpnames 
    = ["+", "-", "*", "/", "||", "&&"
    ,">", ">=", "=", "!=", "<", "<="
    ,":="]

-----------------------------------------------------------------
--  pProg is the topmost parsing function. It looks for a program
--  header "proc main()", followed by the program body.
-----------------------------------------------------------------

pProg :: Parser Program
pProg
    = do 
        procs <- many1 pProc
        return (Program procs)

pProc :: Parser Proc
pProc
    = do
        reserved "proc"
        id <- identifier
        params <- parens (sepBy pParam comma)
        (decls,stmts) <- pBody
        return (Proc id params decls stmts)

pParam :: Parser Param
pParam
    = do
        indic <- pIndic
        ty <- pBaseType
        id <- identifier
        return (Param indic ty id)

pBody :: Parser ([Decl],[Stmt])
pBody
    = do
        decls <- many pDecl
        reserved "begin"
        stmts <- many1 pStmt
        reserved "end"
        return (decls,stmts)       

pDecl = choice [try(pDeclMtx),try(pDeclAry),pDeclVar]
pDeclVar,pDeclAry,pDeclMtx :: Parser Decl
pDeclVar
    = do
        ty <- pBaseType
        id <- identifier
        semi
        return (DeclVar ty id)
pDeclAry
    = do
        ty <- pBaseType
        id <- identifier
        int <- squares integer
        semi
        return (DeclArray ty id (fromInteger int :: Int))
pDeclMtx
    = do
        ty <- pBaseType
        id <- identifier
        (int1,int2) <- squares pDeclMatrix 
        semi
        return (DeclMatrix ty id int1 int2)

pDeclMatrix :: Parser (Int,Int)
pDeclMatrix
    = do
        int1 <- integer 
        comma 
        int2 <- integer
        return (fromInteger int1 :: Int, fromInteger int2 :: Int)

pStmt = choice [pStmtAssign,pStmtRead,pStmtWrite,pStmtCall,pStmtIf,pStmtIfElse,pStmtWhile]
pStmtAssign,pStmtRead,pStmtWrite,pStmtCall,pStmtIf,pStmtIfElse,pStmtWhile :: Parser Stmt
pStmtAssign 
    = do
        var <- pVar
        reservedOp ":="
        expr <- pExpr
        semi
        return (Assign var expr)

pStmtRead
    = do
        reserved "read"
        var <- pVar
        semi
        return (Read var)

pStmtWrite
    = do
        reserved "write"
        expr <- pExpr
        semi
        return (Write expr)

pStmtCall
    = do
        reserved "call"
        id <- identifier
        exprs <- parens (sepBy pExpr comma)
        semi
        return (Call id exprs)

pStmtIf
    = do
        reserved "if"
        con <- pExpr
        reserved "then"
        stmts <- many1 pStmt
        reserved "fi"
        return (If con stmts)

pStmtIfElse
    = do
        reserved "if"
        con <- pExpr
        reserved "then"
        stmts1 <- many1 pStmt
        reserved "else"
        stmts2 <- many1 pStmt
        reserved "fi"
        return (IfElse con stmts1 stmts2)

pStmtWhile
    = do
        reserved "while"
        con <- pExpr
        reserved "do"
        stmts <- many1 pStmt
        reserved "od"
        return (While con stmts)

pIndic :: Parser Indic
pIndic
    = do { reserved "val"; return Val }
    <|>
    do { reserved "ref"; return Ref }

pBaseType :: Parser BaseType
pBaseType
  = do { reserved "bool"; return BoolType }
    <|>
    do { reserved "int"; return IntType }
    <|>
    do { reserved "float"; return FloatType }

pVar = choice[try(pVarMa),try(pVarAr),pVarId]

pVarId,pVarAr,pVarMa :: Parser Var
pVarId
    = do
        id <- identifier <?> "Illegal identifier name."
        return (Id id)
pVarAr
    = do
        id <- identifier
        expr <- squares pExpr
        return (Array id expr)
pVarMa 
    = do
        id <- identifier
        (expr1,expr2) <- squares pMatrix
        return (Matrix id expr1 expr2)

pMatrix :: Parser (Expr,Expr)
pMatrix
    = do 
        expr1 <- pExpr
        comma
        expr2 <- pExpr
        return (expr1,expr2)

pExprOp = choice [parens pExprOp, pExprConst, pExprVar]
pExpr, pExprConst, pExprVar :: Parser Expr
pExpr = buildExpressionParser precedence pExprOp

precedence = [ [Prefix (reservedOp "-" >> return (Unary Minus))]
             , [Infix (reservedOp "*" >> return (Binary Mul)) AssocLeft, 
                Infix (reservedOp "/" >> return (Binary Div)) AssocLeft]
             , [Infix (reservedOp "+" >> return (Binary Add)) AssocLeft, 
                Infix (reservedOp "-" >> return (Binary Sub)) AssocLeft]
             , [Infix (reservedOp "=" >> return (Binary Eq)) AssocNone,
                Infix (reservedOp "!=" >> return (Binary NotEq)) AssocNone,
                Infix (reservedOp "<" >> return (Binary Lt)) AssocNone,
                Infix (reservedOp "<=" >> return (Binary LtEq)) AssocNone,
                Infix (reservedOp ">" >> return (Binary Gt)) AssocNone,
                Infix (reservedOp ">=" >> return (Binary GtEq)) AssocNone]
             , [Prefix (reservedOp "!" >> return (Unary Neg))]
             , [Infix (reservedOp "&&" >> return (Binary And)) AssocLeft]
             , [Infix (reservedOp "||" >> return (Binary Or)) AssocLeft]
             ]

pExprConst = do{const <- pConst;return (Const const)}
pExprVar = do{var <- pVar;return (Var var)}

pConst = choice [pBool, try(pFloat), pInt, pString]
pBool, pInt, pFloat, pString :: Parser Const
pBool =
    do {reserved "true";return (BoolConst True)}
    <|>
    do {reserved "false";return (BoolConst False)}
pInt =
    do 
        int <- integer
        return (IntConst (fromInteger int :: Int))
pFloat =
    do 
        f <- float
        return (FloatConst (realToFrac f :: Float))
pString
    = do
        char '"'
        str <- many (satisfy (/= '"'))
        char '"'
        return (StrConst str)
        <?>
        "string"
-----------------------------------------------------------------
-- main
-----------------------------------------------------------------

pMain :: Parser Program
pMain
    = do
        whiteSpace
        p <- pProg
        eof
        return p

main :: IO ()
main
    = do { progname <- getProgName
        ; args <- getArgs
        ; checkArgs progname args
        ; input <- readFile (head args)
        ; let output = runParser pMain 0 "" input
        ; case output of
            Right ast -> print ast
            Left  err -> do { putStr "Parse error at "
                            ; print err
                            }
        }

checkArgs :: String -> [String] -> IO ()
checkArgs _ [filename]
    = return ()
checkArgs progname _
    = do { putStrLn ("Usage: " ++ progname ++ " filename\n\n")
        ; exitWith (ExitFailure 1)
        }

