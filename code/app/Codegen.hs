import Parser

-- Beginning the code generation for Fafel to Yul. 
-- Much more research to be done on Yul constructs and how to map fafel to yul.

genYulLit :: Literal -> String
genYulLit (IntLit s) = genYulInt s
genYulLit (FloatLit s) = genYulFloat s
genYulLit (BoolLit s) = genYulBool s
genYulLit (AddressLit s) = genYulAddress s

genYulInt :: IntLit -> String
genYulInt (IntLit s) = show s

genYulFloat :: FloatLit -> String
genYulFloat (FloatLit s) =  show s

genYulBool :: BoolLit -> String
genYulBool (BoolLit True) = "true"
genYulBool (BoolLit False) = "false"

genYulAddress :: AddressLit -> String
genYulAddress (AddressLit s) = "address(" ++ s ++ ")"

genYulBinaryOp :: BinaryOperator -> String
genYulBinaryOp Plus = "add"
genYulBinaryOp Minus = "sub"
genYulBinaryOp Times = "mul"
genYulBinaryOp Divide = "div"
genYulBinaryOp And = "and"
genYulBinaryOp Or = "or"
genYulBinaryOp In = "mload"

genYulCompareOp :: CompareOperator -> String
genYulCompareOp Less = "lt"
genYulCompareOp Greater = "gt"
genYulCompareOp LessEq = "le"
genYulCompareOp GreaterEq = "ge"
genYulCompareOp Equal = "eq"
genYulCompareOp NotEqual = "ne"

genYulExpr :: Expr -> String
genYulExpr (BinaryExpr s) = genYulBinaryExpr s 
genYulExpr (CompareExpr s) = genYulCompareExpr s
genYulExpr (Literal s) =  genYulLit s


genYulBinaryExpr :: BinaryExpr -> String
genYulBinaryExpr (BinaryExpr op e1 e2) = 
    genYulBinaryOp op ++ "(" ++ genYulLit e1 ++ "," ++ genYulLit e2 ++ ")"

genYulCompareExpr :: CompareExpr -> String
genYulCompareExpr (CompareExpr op e1 e2) = 
    genYulCompareOp op ++ "(" ++ genYulLit e1 ++ "," ++ genYulLit e2 ++ ")"

genYulFuncCall :: FunctionCall -> String
genYulFuncCall (FunctionCall name args) = 
    "function " ++ name ++ "(" ++ (intercalate ", " $ map show args) ++ ")"

slotCounter :: IORef Int
slotCounter = unsafePerformIO $ newIORef 0

getSlot :: IO Int
getSlot = do
    counter <- readIORef slotCounter
    writeIORef slotCounter (counter + 1)
    return counter

genYulMapping :: MapDecl -> String
-- should open a slot in storage for the mapping index
-- and then store the mapping name and position in a symbol table 
genYulMapping name _ = 
  let slot = getSlot
  in "sstore(" ++ show slot ++ ", 0)"

genYulMappingExpr :: MapExpr -> String
genYulMappingExpr name key = 



genYulMappingAssign :: MapAssignExpr -> String
genYulMappingAssign 

genYulList :: ListType -> String
genYulList l =
  let slot = getSlot
  in "sstore(" ++ show slot ++ ", 0)"


