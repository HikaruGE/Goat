module MyTest where

import GoatAST
import qualified Data.Map as Map

data VarInfo
    = VarInfo BaseType (Maybe Indic)
        deriving(Show)


isBinExpr :: Expr -> Bool
isBinExpr (Binary _ _ _) = True
isBinExpr _ = False

test1 :: Int -> Int
test1 x = 
    let x' = add1 x
        y' = mul2 x'
    in
        y'
    
-- concatMap :: (a -> [b]) -> [a] -> [b]

testConcatMap strLst str = concatMap (strAddColon str) strLst 
strAddColon str1 str2 = "zijidecode"++str1

add1 :: Int -> Int
add1 x = x+1
mul2 :: Int -> Int
mul2 x = x*2


boolToInt :: Bool -> Int
boolToInt True = 1 
boolToInt False = 0

data BinOpClass 
    = Arithmetic | Comparision | Logic

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