module Codegen where

import AST
import Parser
import Data.Map (Map)
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative
import Crypto.Hash (hash, Digest)
import Crypto.Hash.Algorithms (Keccak_256)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Base16 as B16

{-
    This file is used to generate Yul code from an AST
    We have a generator for each type of AST node
    We build the Yul code by combining these generators
    And then we have a generator for the entire contract

    We use the State monad to keep track of the symbol table
    The symbol table maps variable names to their slot in memory
    We use the State monad to keep track of the current slot for storage
-}

type SymbolTable = Map String Int
type FafelState a = StateT SymbolTable IO a

initialSymbolTable :: SymbolTable
initialSymbolTable = Map.empty

getSlot :: FafelState Int
getSlot = do
    sym <- get
    let currentSlot = Map.size sym
    return (currentSlot + 1)

addToSymbolTable :: String -> Int -> FafelState ()
addToSymbolTable name slot = do
    sym <- get
    put $ Map.insert name slot sym

lookupInSymbolTable :: String -> FafelState (Maybe Int)
lookupInSymbolTable name = do
    sym <- get
    return $ Map.lookup name sym

genYulLit :: Literal -> FafelState String
genYulLit (IntLit s) = return $ show $ s * 1000000000000000000
genYulLit (FloatLit s) = return $ (show $ fst s) ++ (show $ snd s) ++ replicate (18 - length (show $ snd s)) '0'
genYulLit (BoolLit True) = return $ "1"
genYulLit (BoolLit False) = return $ "0"
genYulLit (AddressLit s) = return $ "0x" ++ s

genYulBinaryOp :: BinaryOperator -> FafelState String
genYulBinaryOp Plus = return $ "add"
genYulBinaryOp Minus = return $ "sub"
genYulBinaryOp Times = return $ "mul"
genYulBinaryOp Divide = return $  "div"
genYulBinaryOp And = return $ "and"
genYulBinaryOp Or = return $ "or"
genYulBinaryOp StateVarAssign = return $ "sstore"
genYulBinaryOp Less = return $ "lt"
genYulBinaryOp Greater = return $ "gt"
genYulBinaryOp LessEq = return $ "le"
genYulBinaryOp GreaterEq = return $ "ge"
genYulBinaryOp Equal = return $ "eq"
genYulBinaryOp NotEqual = return $ "ne"

genYulExpr :: Expr -> FafelState String
genYulExpr (FunctionCall n a) = genYulFuncCall (FunctionCall n a)
genYulExpr (BinaryExpr op e1 e2) = genYulBinaryExpr (BinaryExpr op e1 e2)
genYulExpr (Literal s) =  genYulLit s
genYulExpr (Var s) = genYulVar (Var s)
genYulExpr (IfExpr cond e1 e2) = genYulIfExpr (IfExpr cond e1 e2)
genYulExpr (FunctionExpr name args expr) = genYulFunctionExpr (FunctionExpr name args expr)
genYulExpr (MapExpr name key) = genYulMappingExpr (MapExpr name key)
genYulExpr (MapAssignExpr name key val) = genYulMappingAssign (MapAssignExpr name key val)
genYulExpr (ListExpr name index) = genYulListExpr (ListExpr name index)
genYulExpr (ListAssignExpr name index val) = genYulListAssign (ListAssignExpr name index val)

genYulWrapper :: Expr -> FafelState String
genYulWrapper expr = do
    expr' <- genYulExpr expr
    return $ "result := " ++ expr'

genYulBinaryExpr :: Expr -> FafelState String
genYulBinaryExpr (BinaryExpr op e1 e2) = do
  e1' <- genYulExpr e1
  e2' <- genYulExpr e2
  case op of
    StateVarAssign -> do
      slot <- lookupInSymbolTable $ getVarName e1
      case slot of
        Just s -> return $ "sstore(" ++ show s ++ ", " ++ e2' ++ ")"
        Nothing -> error "Variable not found in symbol table"
    _ -> do
      op' <- genYulBinaryOp op
      return $ op' ++ "(" ++ e1' ++ ", " ++ e2' ++ ")"

getVarName :: Expr -> String
getVarName (Var name) = name

genYulFuncCall :: Expr -> FafelState String
genYulFuncCall (FunctionCall name args) = do
    args' <- mapM genYulExpr args
    return $ name ++ "(" ++ intercalate ", " args' ++ ")"


--genYulIfExpr :: Expr -> String

genYulVar :: Expr -> FafelState String
genYulVar (Var name) = do
    check <- lookupInSymbolTable name
    case check of
        Just check -> return $ "sload(" ++ show check ++ ")"
        Nothing -> return $ name

genYulStateVariable :: StateVariable -> FafelState String
genYulStateVariable (MapDecl name ty) =
    storeMapping (MapDecl name ty)
genYulStateVariable (ListDecl name ty) =
    storeList (ListDecl name ty)
genYulStateVariable (IntDecl name ty) =
    storeInt (IntDecl name ty)
genYulStateVariable (FloatDecl name ty) =
    storeFloat (FloatDecl name ty)
genYulStateVariable (BoolDecl name ty) =
    storeBool (BoolDecl name ty)
genYulStateVariable (AddressDecl name ty) =
    storeAddress (AddressDecl name ty)

-- this function gets the next slot in storage and stores an int.
storeInt :: StateVariable -> FafelState String
storeInt (IntDecl name ty) = do
    slot <- getSlot
    addToSymbolTable name slot
    return $ "sstore(" ++ show slot ++ ", 0)\n"

-- this function gets the next slot in storage and stores a float.
storeFloat :: StateVariable -> FafelState String
storeFloat (FloatDecl name ty) = do
    slot <- getSlot
    addToSymbolTable name slot
    return $ "sstore(" ++ show slot ++ ", 0\n"

-- this function gets the next slot in storage and stores a bool value.
storeBool :: StateVariable -> FafelState String
storeBool (BoolDecl name ty) = do
    slot <- getSlot
    addToSymbolTable name slot
    return $ "sstore(" ++ show slot ++ ", 0)\n"

-- this function gets the next slot in storage and stores an address.
storeAddress :: StateVariable -> FafelState String
storeAddress (AddressDecl name ty) = do
    slot <- getSlot
    addToSymbolTable name slot
    return $ "sstore(" ++ show slot ++ ", 0)\n"

storeList :: StateVariable -> FafelState String
storeList (ListDecl name val) = do
    slot <- getSlot
    addToSymbolTable name slot
    return $ "sstore(" ++ show slot ++ ", 0)\n"


storeMapping :: StateVariable -> FafelState String
storeMapping (MapDecl name ty) = do
    slot <- getSlot
    addToSymbolTable name slot
    return $ "sstore(" ++ show slot ++ ", 0)\n"


genYulContract :: Contract -> FafelState String
genYulContract (Contract name stateVariables functions) = do
    stateVars <- mapM genYulStateVariable stateVariables
    funcs <- mapM genYulFunction functions
    selectors <- mapM functionSelector functions
    dispatcher <- mapM genYulFunctionSelector selectors
    let contract = "object " ++ show name ++  " {\n" ++ "  code {\n" ++ (concat stateVars) ++ "\ndatacopy(0, dataoffset(\"runtime\"), datasize(\"runtime\"))\nreturn(0, datasize(\"runtime\"))\n" ++ "\n  }\n" ++ "  object \"runtime\" {\n    code {\n\tlet selector := shr(0xe0, calldataload(0x00))\n\tswitch selector \n" ++ (concat dispatcher) ++ "\tdefault {\n\trevert(0, 0)\n}\nfunction returnUint(x) {\nmstore(0x00, x)\nreturn(0x00, 0x20)\n}\n" ++ "\n\n" ++ (concat funcs) ++ "\n}\n}\n\n}"
    return contract

-- This function gets the name and argument types of a function and hashes it in  the form func_name(type, type...typen)
-- we only want the first 8 bytes of the hash
functionSelector :: Function -> FafelState (String, String,  String)
functionSelector (Function name args returnType expr) = do
    let hash = if null args then keccak256Hex $ name else keccak256Hex $ name ++ "(" ++ (intercalate "," (map show args)) ++ ")"
    return (name, show args, "0x" ++ take 8 hash)

genYulFunctionSelector :: (String, String, String) -> FafelState String
genYulFunctionSelector (name, args, hash) = do
    let argList = if args == "[]" then [] else splitOn "," args
    let argString = if null argList then "" else intercalate ", " $ zipWith (\arg i -> "calldataload(add(0x04, mul(" ++ show i ++ ", 0x20)))") argList [0..]
    return $ "\tcase " ++ hash ++ "\n\t{ " ++ "returnUint(" ++ name ++ "(" ++ argString ++ ")) }\n"

    

-- This function take a function node and maps it to its respective yul code.
genYulFunction :: Function -> FafelState String
genYulFunction (Function name _ returnType expr) = do
    if returnType == StateType
        then do
            expr' <- genYulFunctionExpr' expr
            return $ "function " ++ name ++ "(" ++ expr'
        else do
            expr' <- genYulFunctionExpr expr
            return $ "function " ++ name ++ "(" ++ expr'

genYulFunctionExpr :: Expr -> FafelState String
genYulFunctionExpr (FunctionExpr name args expr@(Var s)) = do
    args' <- mapM genYulExpr args
    expr' <- genYulExpr expr
    return $ (intercalate ", " args') ++ ")" ++ " -> result { result := " ++  expr' ++ " }\n"
genYulFunctionExpr (FunctionExpr name args expr@(BinaryExpr _ _ _)) = do
  args' <- mapM genYulExpr args
  expr' <- genYulWrapper expr
  return $ (intercalate ", " args') ++ ")" ++ " -> result { " ++  expr' ++ " }\n"

genYulFunctionExpr (FunctionExpr name args expr) = do
  args' <- mapM genYulExpr args
  expr' <- genYulExpr expr
  return $ (intercalate ", " args') ++ ")" ++ " -> result { " ++  expr' ++ " }\n"


genYulFunctionExpr' :: Expr -> FafelState String
genYulFunctionExpr' (FunctionExpr name args (BinaryExpr StateVarAssign left right)) = do
    args' <- mapM genYulExpr args
    expr' <- genYulExpr (BinaryExpr StateVarAssign left right)
    return $ (intercalate ", " args') ++ ") -> result { "++ expr' ++ "\nresult := 0\n}\n"
genYulFunctionExpr' (FunctionExpr name args expr@(BinaryExpr _ _ _)) = do
    args' <- mapM genYulExpr args
    expr' <- genYulExpr expr
    return $ (intercalate ", " args') ++ ") -> result " ++ "{ \n result := " ++ expr' ++ "\n}\n"
genYulFunctionExpr' (FunctionExpr name args expr) = do
    args' <- mapM genYulExpr args
    expr' <- genYulExpr expr
    return $ (intercalate ", " args') ++ ") -> result " ++ "{ \n" ++ expr' ++ "\nresult := 0\n}\n"

genYulIfExpr :: Expr -> FafelState String
genYulIfExpr (IfExpr cond thenExpr elseExpr) = do
    cond' <- genYulExpr cond
    thenExpr' <- genYulExpr thenExpr
    elseExpr' <- genYulExpr elseExpr
    return $ "switch " ++ cond' ++ "\ncase true { result := " ++ thenExpr' ++ " } \ndefault { result := " ++ elseExpr' ++ " }\n"

genYulMappingExpr :: Expr -> FafelState String
genYulMappingExpr (MapExpr name key) = do
    slot <- lookupInSymbolTable name
    key' <- genYulExpr key
    case slot of
        Just slot -> return $ "\n        mstore(0x00, " ++ show slot ++ ")\n        mstore(0x20, " ++ key' ++ ")\n" ++ "        result := sload(keccak256(0x00, 0x40))"
        Nothing -> return $ error $ "Error: " ++ name ++ " not found in symbol table."

genYulMappingAssign :: Expr -> FafelState String
genYulMappingAssign (MapAssignExpr name key val) = do
    slot <- lookupInSymbolTable name
    key' <- genYulExpr key
    val' <- genYulExpr val
    case slot of
        Just slot -> return $ "\n        mstore(" ++ "0x00, " ++ show slot ++ ")\n        mstore(0x40, " ++ key' ++ ")\n        sstore(keccak256(0x00, 0x60), " ++ val' ++ ")\n\n"
        Nothing -> return $ error $ "Error: " ++ name ++ " not found in symbol table."

genYulListExpr :: Expr -> FafelState String
genYulListExpr (ListExpr name index) = do
    slot <- lookupInSymbolTable name
    index' <- genYulExpr index
    case slot of
        Just slot -> return $ "\n        mstore(0x00, " ++ show slot ++ ")\n        result := sload(add(keccak256(0x00, 0x20), " ++ index' ++ "))"
        Nothing -> return $ error $ "Error: " ++ name ++ " not found in symbol table."

genYulListAssign :: Expr -> FafelState String
genYulListAssign (ListAssignExpr name index value) = do
    slot <- lookupInSymbolTable name
    val' <- genYulExpr value
    case slot of
        Just slot -> return $ "\n        mstore(" ++ "0x40, " ++ show slot ++ ")\n        \n        sstore(keccak256(0x40, 0x80), " ++ val' ++ ")\n"
        Nothing -> return $ error $ "Error: " ++ name ++ " not found in symbol table."

keccak256Hex :: String -> String
keccak256Hex s = show (hash (C8.pack s) :: Digest Keccak_256)
