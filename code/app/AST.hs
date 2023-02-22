module AST
where

-- AST for Fafel source code
-- These constructors are used to represent the AST

data Contract = Contract String [StateVariable] [Function]
  deriving (Show)

data DataType = IntType | FloatType | BoolType | AddressType | ListType DataType | MapType (DataType, DataType) | StateType
  deriving (Show, Eq)

data StateVariable = MapDecl String DataType | ListDecl String DataType | IntDecl String DataType | FloatDecl String DataType | BoolDecl String DataType | AddressDecl String DataType
  deriving (Show)
  
data Function = Function String [DataType] DataType Expr
  deriving (Show)

data Expr = FunctionCall String [Expr] | 
            FunctionExpr String [Expr] Expr |
            BinaryExpr BinaryOperator Expr Expr |
            MapAssignExpr String Expr Expr | 
            ListAssignExpr String Expr Expr | 
            IfExpr Expr Expr Expr |
            MapExpr String Expr |
            ListExpr String Expr |
            Var String | 
            Literal Literal
            deriving (Show, Eq)

data Literal = IntLit Integer | FloatLit (Integer, Integer) | BoolLit Bool | AddressLit String
  deriving (Show, Eq)

data BinaryOperator = StateVarAssign | Plus | Minus | Times | Divide | And | Or | In | Less | Greater | LessEq | GreaterEq | Equal | NotEqual
  deriving (Show, Eq)

data UnaryOperator = Not
  deriving (Show, Eq)
