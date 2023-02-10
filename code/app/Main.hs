module Main where

-- main.hs
import Parser
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Codegen
import Control.Monad.State

main :: IO ()
main = do
  contents <- readFile "example.fafel"
  case runParser contractParser () "" contents of
    Left error -> putStrLn $ show error
    Right result -> do
        putStrLn $ show result
        (output, _) <- runStateT (genYulContract result) initialSymbolTable
        putStrLn output

  
  
