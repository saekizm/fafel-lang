module Parser where

import Text.Parsec
import Text.Parsec.String 
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import Lexer
import AST

-- This file is used to parse Fafel source code into an AST
-- we have a parser for each type of AST node
-- we build the AST by combining these parsers
-- and then we have a parser for the entire contract

----------------------  state variables ------------------------------------
----------------------------------------------------------------------------

stateVariableParser :: Parser StateVariable
stateVariableParser = try mapDeclParser
            <|> try listDeclParser
            <|> try addressDeclParser
            <|> try floatDeclParser
            <|> try intDeclParser
            <|> try boolDeclParser

intDeclParser :: Parser StateVariable
intDeclParser = do
  name <- identifier
  spaces
  char ':'
  spaces
  ty <- intTypeParser
  return (IntDecl name ty)

floatDeclParser :: Parser StateVariable
floatDeclParser = do
  name <- identifier
  spaces
  char ':'
  spaces
  ty <- floatTypeParser
  return (FloatDecl name ty)

boolDeclParser :: Parser StateVariable
boolDeclParser = do
  name <- identifier
  spaces
  char ':'
  spaces
  ty <- boolTypeParser
  return (BoolDecl name ty)

addressDeclParser :: Parser StateVariable
addressDeclParser = do
  name <- identifier
  spaces
  char ':'
  spaces
  ty <- addressTypeParser
  return (AddressDecl name ty)

mapDeclParser :: Parser StateVariable
mapDeclParser = do
  name <- identifier
  spaces
  char ':'
  spaces
  ty <- mapTypeParser
  return (MapDecl name ty)

listDeclParser :: Parser StateVariable
listDeclParser = do
  name <- identifier
  spaces
  char ':'
  spaces
  ty <- listTypeParser
  return (ListDecl name ty)
----------------------------------------------------------------------------
----------------------------------------------------------------------------



---------------------- Type Parsers ----------------------------------------
----------------------------------------------------------------------------


--type parsers--
dataTypeParser :: Parser DataType
dataTypeParser = try stateTypeParser
                <|> try mapTypeParser
                <|> try listTypeParser
                <|> try addressTypeParser
                <|> try floatTypeParser
                <|> try intTypeParser
                <|> try boolTypeParser

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
  char '['
  ty <- dataTypeParser
  char ']'
  return (ListType ty)

mapTypeParser :: Parser DataType
mapTypeParser = do
    string "mapping"
    char '('
    ty1 <- dataTypeParser
    string "->"
    ty2 <- dataTypeParser
    char ')'
    return (MapType (ty1, ty2))

stateTypeParser :: Parser DataType
stateTypeParser = do
    string "state"
    return StateType

----------------------------------------------------------------------------
----------------------------------------------------------------------------

parseString :: Parser Expr -> String -> Either ParseError Expr
parseString e s = parse (e <* eof) "" s

parseFile :: FilePath -> IO (Either ParseError Contract)
parseFile file = parseFromFile contractParser file


---------------------- literal parsers ------------------------------------
----------------------------------------------------------------------------


literalParser :: Parser Expr
literalParser = do
    try addressLiteralParser
    <|> try floatLiteralParser
    <|> try intLiteralParser
    <|> try boolLiteralParser

intLiteralParser :: Parser Expr
intLiteralParser = Literal . IntLit <$> integer

boolLiteralParser :: Parser Expr
boolLiteralParser = do
  try (reserved "true" >> return (Literal (BoolLit True)))
    <|> (reserved "false" >> return (Literal (BoolLit False)))

addressLiteralParser :: Parser Expr
addressLiteralParser = do
  string "0x"
  address <- count 40 (satisfy isHexDigit)
  return (Literal (AddressLit address))

floatLiteralParser :: Parser Expr
floatLiteralParser = Literal . FloatLit <$> float

isHexDigit :: Char -> Bool
isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

----------------------------------------------------------------------------
----------------------------------------------------------------------------



----------------------- Expression Parsers ----------------------------------
----------------------------------------------------------------------------


binary s f assoc = Ex.Infix (reservedOp s *> spaces *> return (BinaryExpr f)) assoc


table = [
         [binary "*" Times Ex.AssocLeft,
          binary "/" Divide Ex.AssocLeft]
        ,[binary "+" Plus Ex.AssocLeft,
          binary "-" Minus Ex.AssocLeft]
        ,[binary "==" Equal Ex.AssocNone,
          binary "<" Less Ex.AssocNone,
          binary ">" Greater Ex.AssocNone,
          binary "<=" LessEq Ex.AssocNone,
          binary ">=" GreaterEq Ex.AssocNone]
        ,[binary "&&" And Ex.AssocLeft,
          binary "||" Or Ex.AssocLeft]
        ,[binary ":=" StateVarAssign Ex.AssocNone]]

opExpr :: Parser Expr
opExpr = Ex.buildExpressionParser table (term <* spaces)

expr :: Parser Expr
expr = try mapAssignParser
        <|> try listAssignParser
        <|> try opExpr

term :: Parser Expr
term = try functionCallParser
        <|> try mapAssignParser
        <|> try listAssignParser
        <|> try ifExprParser
        <|> try mapExprParser
        <|> try listExprParser
        <|> try literalParser
        <|> try variable
        <|> parens opExpr

ifExprParser :: Parser Expr
ifExprParser = do
  reserved "if"
  cond <- expr
  reserved "then"
  thenExpr <- expr
  reserved "else"
  elseExpr <- expr
  return (IfExpr cond thenExpr elseExpr)

variable :: Parser Expr
variable = do
  var <- identifier
  return $ Var var

functionCallParser :: Parser Expr
functionCallParser = do
  functionName <- identifier
  args <- (parens (commaSep variable))
  return (FunctionCall functionName args)

--works
mapExprParser :: Parser Expr
mapExprParser = do
  name <- identifier
  char '{'
  key <- expr
  char '}'
  return (MapExpr name key)

mapAssignParser :: Parser Expr
mapAssignParser = do
  name <- identifier
  char '{'
  key <- expr
  char '}'
  spaces
  char '='
  spaces 
  val <- expr
  return (MapAssignExpr name key val)

--works
listExprParser :: Parser Expr
listExprParser = do
  name <- identifier
  char '['
  index <- expr
  char ']'
  return (ListExpr name index)

listAssignParser :: Parser Expr
listAssignParser = do
  name <- identifier
  char '['
  index <- intLiteralParser
  char ']'
  spaces
  char '='
  spaces
  val <- literalParser
  return (ListAssignExpr name index val)

-- tested and works!
functionExprParser :: Parser Expr
functionExprParser = do
  var <- identifier
  spaces
  args <- sepEndBy expr spaces
  string "="
  spaces
  ex <- expr
  return (FunctionExpr var args ex)

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
    whitespace
    expr <- functionExprParser
    skipMany (oneOf " \t\r\n")
    return (Function name inputTypes outputType expr)

----------------------------------------------------------------------------
----------------------------------------------------------------------------




---------------------- Contract Parsers ------------------------------------
----------------------------------------------------------------------------
contractParser :: Parser Contract
contractParser = do
  reserved "fafel"
  spaces
  contractName <- identifier
  spaces
  char '{'
  whitespace
  stateVars <- stateParser
  skipMany (oneOf " \n\t\r")
  functions <- manyTill functionParser (string "}")
  return (Contract contractName stateVars functions)




stateParser :: Parser [StateVariable]
stateParser = do
    reserved "state"
    whitespace
    stateVars <- braces (sepEndBy stateVariableParser whitespace)
    return (stateVars)

----------------------------------------------------------------------------
----------------------------------------------------------------------------

