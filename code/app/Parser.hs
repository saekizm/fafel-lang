import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import System.IO
data Contract = Contract String [StateVariable] [Function]
  deriving (Show)

data StateVariable = StateVariable String DataType
  deriving (Show)

data DataType = IntType | FloatType | BoolType | AddressType | ListType | MapType | StateType
  deriving (Show)

data Function = Function String [DataType] DataType Expr
  deriving (Show)

data Expr = Literal Literal | Var String | FunctionCall String [Expr] | FunctionExpr String [Expr] Expr | IfExpr Expr Expr Expr | MapExpr [(Expr, Expr)] |ListExpr [Expr] | BinaryExpr BinaryOperator Expr Expr | UnaryExpr UnaryOperator Expr | CompareExpr CompareOperator Expr Expr
  deriving (Show)

data Literal = IntLit Integer | FloatLit Float | BoolLit Bool | AddressLit String
  deriving (Show)

data BinaryOperator = Plus | Minus | Times | Divide | And | Or | In
  deriving (Show)

data UnaryOperator = Not
  deriving (Show)

data CompareOperator = Less | Greater | LessEq | GreaterEq | Equal | NotEqual
  deriving (Show)


identifier :: Parser String
identifier = do
    firstChar <- letter
    rest <- many (letter <|> digit <|> char '.')
    return (firstChar:rest)

literalParser :: Parser Expr
literalParser = do
  try floatLiteralParser
    <|> try intLiteralParser
    <|> try boolLiteralParser
    <|> try addressLiteralParser

intLiteralParser :: Parser Expr
intLiteralParser = do
  n <- many1 digit
  return (Literal (IntLit (read n)))

boolLiteralParser :: Parser Expr
boolLiteralParser = do
  try (string "true" >> return (Literal (BoolLit True)))
    <|> (string "false" >> return (Literal (BoolLit False)))

addressLiteralParser :: Parser Expr
addressLiteralParser = do
  string "0x"
  address <- count 40 (satisfy isHexDigit)
  return (Literal (AddressLit address))

floatLiteralParser :: Parser Expr
floatLiteralParser = do
  n <- many1 digit
  char '.'
  d <- many1 digit
  return (Literal (FloatLit (read (n ++ "." ++ d))))

isHexDigit :: Char -> Bool
isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')


--type parsers--
dataTypeParser :: Parser DataType
dataTypeParser = try intTypeParser
                <|> try floatTypeParser
                <|> try boolTypeParser
                <|> try addressTypeParser
                <|> try listTypeParser
                <|> try mapTypeParser
                <|> try stateTypeParser

floatTypeParser :: Parser DataType
floatTypeParser = do
  string "float"
  return FloatType

intTypeParser :: Parser DataType
intTypeParser = do
  string "int"
  return IntType


boolTypeParser :: Parser DataType
boolTypeParser = do
  string "bool"
  return BoolType

addressTypeParser :: Parser DataType
addressTypeParser = do
  string "address"
  return AddressType

listTypeParser :: Parser DataType
listTypeParser = do
  string "list"
  return ListType

mapTypeParser :: Parser DataType
mapTypeParser = do
    string "map"
    return MapType

stateTypeParser :: Parser DataType
stateTypeParser = do
    string "state"
    return StateType


--expression parsers--

varExprParser :: Parser Expr
varExprParser = do
  varName <- identifier
  return (Var varName)

--works
listExprParser :: Parser Expr
listExprParser = do
  char '['
  elements <- sepBy exprParser (string ", ")
  char ']'
  return (ListExpr  elements)

--works
mapExprParser :: Parser Expr
mapExprParser = do
  char '{'
  spaces
  key <- exprParser
  spaces
  char ':'
  spaces
  value <- exprParser
  spaces
  char '}'
  return (MapExpr [(key, value)])

--works
exprParser :: Parser Expr
exprParser = try functionExprParser
           <|> try functionCallParser
           <|> try ifExprParser
           <|> try binaryExprParser
           <|> try compareExprParser
           <|> try unaryExprParser
           <|> try listExprParser
           <|> try mapExprParser
           <|> try varExprParser
           <|> try literalParser


--works
ifExprParser :: Parser Expr
ifExprParser = do
  string "if"
  spaces
  condition <- try binaryExprParser <|> try literalParser <|> try unaryExprParser
  newline
  string "then"
  spaces
  thenExpr <- try binaryExprParser <|> try literalParser <|> try unaryExprParser
  newline
  string "else"
  spaces
  elseExpr <- try binaryExprParser <|> try literalParser <|> try unaryExprParser
  return (IfExpr condition thenExpr elseExpr)

--works
binaryExprParser :: Parser Expr
binaryExprParser = do
    term <- try intLiteralParser <|> try floatLiteralParser <|> try varExprParser
    spaces
    rest <- many (do { op <- binaryOperatorParser; spaces; term <- try intLiteralParser <|> try floatLiteralParser <|> try varExprParser; spaces; return (op, term) } )
    return $ foldl (\acc (op, term) -> BinaryExpr op acc term) term rest

--works
unaryExprParser :: Parser Expr
unaryExprParser = do
  op <- unaryOperatorParser
  e <- exprParser
  return $ UnaryExpr op e

compareExprParser :: Parser Expr
compareExprParser = do
    term <- try intLiteralParser <|> try floatLiteralParser <|> varExprParser
    spaces
    rest <- many (do { op <- compareOperatorParser; spaces; term <- try intLiteralParser <|> try floatLiteralParser <|> varExprParser; spaces; return (op, term) } )
    return $ foldl (\acc (op, term) -> CompareExpr op acc term) term rest





--function parsers
--works
functionCallParser :: Parser Expr
functionCallParser = do
  functionName <- identifier
  char '('
  args <- sepBy exprParser (char ',')
  char ')'
  return (FunctionCall functionName args)

-- tested and works!
functionExprParser :: Parser Expr
functionExprParser = do
  var <- identifier
  spaces
  args <- sepEndBy (try varExprParser <|> try literalParser <|> try listExprParser <|> try mapExprParser) spaces
  string "="
  spaces
  expr <- exprParser
  return (FunctionExpr var args expr)

--works
functionSigParser :: Parser (String, [DataType], DataType)
functionSigParser = do
    funcName <- identifier
    spaces
    char ':'
    spaces
    args <- between (char '(') (char ')') (sepBy dataTypeParser (string " -> "))
    string " -> "
    returnType <- dataTypeParser
    return (funcName, args, returnType)

-- works
functionParser :: Parser Function
functionParser = do
    (name, inputTypes, outputType) <- functionSigParser
    newline
    expr <- functionExprParser
    return (Function name inputTypes outputType expr)





--operator parsers
binaryOperatorParser :: Parser BinaryOperator
binaryOperatorParser =
  try (string "+" >> return Plus)
  <|> try (string "-" >> return Minus)
  <|> try (string "*" >> return Times)
  <|> try (string "/" >> return Divide)
  <|> try (string "and" >> return And)
  <|> try (string "or" >> return Or)
  <|> try (string "in" >> return In)

unaryOperatorParser :: Parser UnaryOperator
unaryOperatorParser =
  try (string "!" >> return Not)

compareOperatorParser :: Parser CompareOperator
compareOperatorParser = do
    try (string "<" >> return Less)
    <|> try (string ">" >> return Greater)
    <|> try (string "==" >> return Equal)
    <|> try (string "<=" >> return LessEq)
    <|> try (string ">=" >> return GreaterEq)
    <|> try (string "!=" >> return NotEqual)



contractParser :: Parser Contract
contractParser = do
  string "contract"
  spaces
  contractName <- identifier
  spaces
  char '{'
  newline
  stateVars <- stateParser
  skipMany (oneOf " \n\t\r")
  functions <- manyTill functionParser (string "}")
  return (Contract contractName stateVars functions)

--works
stateParser :: Parser [StateVariable]
stateParser = do
  string "state"
  spaces
  char '{'
  newline
  stateVars <- sepBy stateVariableParser (newline)
  skipMany (oneOf " \n\t\r")
  char '}'
  return stateVars

--works
stateVariableParser :: Parser StateVariable
stateVariableParser = do
  varName <- identifier
  spaces
  string ":"
  spaces
  varType <- dataTypeParser
  return (StateVariable varName varType)



main :: IO ()
main = do
    contents <- readFile "example.fafel"
    case parse contractParser "example.fafel" contents of
        Left error -> print error
        Right ast -> print ast