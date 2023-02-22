module Main where

import System.Environment
import System.Exit
import System.Process
import AST
import Lexer
import Parser
import TypeChecker
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Codegen
import Control.Monad.State
import Data.List.Split
import Data.Map as Map (empty)
-- Main function to run compiler
-- Takes a filepath as the first argument
-- If the second argument is "--deploy", it will also deploy the contract

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: fafel <filepath> [--deploy]"
    (filepath:_) -> do
      contents <- parseFile filepath
      case contents of
        Left error -> putStrLn $ show error
        Right result -> do
            writeFile "AST.txt" $ show result
            res <- evalStateT (typecheckContract result) Map.empty
            putStrLn $ "Type checking: " ++ res
            (output, _) <- runStateT (genYulContract result) initialSymbolTable
            writeFile "contract.yul" output
            (_, stdout, stderr) <- readProcessWithExitCode "solc" ["--yul", "contract.yul", "--bin"] ""
            case stderr of
                "" -> do
                    let binary = head $ drop 4 $ lines stdout
                    writeFile "contract.bin" $ concat $ words $ drop 17 binary
                    putStrLn "Compilation successful"
                    putStrLn "Binary saved to contract.bin"
                    if "--deploy" `elem` args
                        then do
                            writeFile "deploy.js" $ generateDeployScript binary
                            putStrLn "Generated deploy script: deploy.js"
                            (_, stdout, stderr) <- readProcessWithExitCode "node" ["deploy.js"] ""
                            case stderr of
                                "" -> putStrLn $ "Deployment successful" ++ stdout
                                _ -> putStrLn $ "Deployment failed: " ++ stderr
                        else return ()
                _ -> putStrLn $ "Compilation failed: " ++ stderr

generateDeployScript :: String -> String
generateDeployScript binary = unlines [
    "require('dotenv').config();",
    "const ethers = require('ethers')",
    "",
    "async function deploy() {",
    "  const provider = new ethers.providers.JsonRpcProvider(process.env.ENDPOINT);",
    "  const wallet = new ethers.Wallet(process.env.PRIVATE_KEY, provider);",
    "  const signer = wallet.connect(provider);",
    "",
    "  const bytecode = '0x" ++ binary ++ "';",
    -- abi is in abi.json, we need it to deploy the contract

    
    "  const contract = new ethers.ContractFactory(abi=[], bytecode, signer);",
    "",
    "  const deployment = await contract.deploy();",
    "  console.log(`Contract deployed at address: ${deployment.address}`);",
    "}",
    "",
    "deploy().catch(error => {",
    "  console.error(error);",
    "  process.exit(1);",
    "});"
  ]

