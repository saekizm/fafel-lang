module Lexer where
import Text.Parsec hiding (Empty)
import Text.Parsec.String
import Text.Parsec.Language
import qualified Text.Parsec.Token as Tok

{-
    This file is used to define the lexer for Fafel source code
    We use Parsec's lexer generator to define the lexer
    We define the language using the emptyDef constructor
    Then we define the tokens we want to use
    Finally we define the lexer using the makeTokenParser function
    We also define some helper functions for parsing
-}

contractLanguage :: LanguageDef ()
contractLanguage = emptyDef {
    Tok.commentStart = "/*",
    Tok.commentEnd = "*/",
    Tok.commentLine = "--",

    Tok.identStart = letter,
    Tok.identLetter = alphaNum,
    Tok.reservedNames = ["fafel", "state", "true", "false", "if", "then", "else"],
    Tok.reservedOpNames = ["+", "-", "*", "/", "and", "or", "not", "<", ">", "<=", ">=", "==", "!=", "="],
    Tok.caseSensitive = True
}

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser contractLanguage

integer :: Parser Integer
integer = Tok.integer lexer

float :: Parser (Integer, Integer)
float = do
    n <- Tok.natural lexer
    char '.'
    d <- Tok.natural lexer
    return (n, d)

parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

whitespace = Tok.whiteSpace lexer

braces :: Parser a -> Parser a
braces = Tok.braces lexer

