import Parser

-- Beginning the code generation for Fafel to Yul. 
-- Much more research to be done on Yul constructs and how to map fafel to yul.

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




