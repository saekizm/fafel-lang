module Codegen where

import Parser
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map
import Control.Monad.State
import Control.Applicative
import Data.Text.Lazy (unpack)


-- work to still be done!
-- must resolve state variables in expressions
-- fix some yul output syntax
-- operator precedence (parser fix)

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
genYulLit (IntLit s) = return $ show s
genYulLit (FloatLit s) = return $ show s
genYulLit (BoolLit True) = return $ "true"
genYulLit (BoolLit False) = return $ "false"
genYulLit (AddressLit s) = return $ "address(" ++ s ++ ")"

genYulBinaryOp :: BinaryOperator -> FafelState String
genYulBinaryOp Plus = return $ "add"
genYulBinaryOp Minus = return $ "sub"
genYulBinaryOp Times = return $ "mul"
genYulBinaryOp Divide =return $  "div"
genYulBinaryOp And = return $ "and"
genYulBinaryOp Or = return $ "or"
genYulBinaryOp In = return $ "mload"

genYulCompareOp :: CompareOperator -> FafelState String
genYulCompareOp Less = return $ "lt"
genYulCompareOp Greater = return $ "gt"
genYulCompareOp LessEq = return $ "le"
genYulCompareOp GreaterEq = return $ "ge"
genYulCompareOp Equal = return $ "eq"
genYulCompareOp NotEqual = return $ "ne"

genYulExpr :: Expr -> FafelState String
genYulExpr (FunctionCall n a) = genYulFuncCall (FunctionCall n a)
genYulExpr (BinaryExpr op e1 e2) = genYulBinaryExpr (BinaryExpr op e1 e2)
genYulExpr (CompareExpr op e1 e2) = genYulCompareExpr (CompareExpr op e1 e2)
genYulExpr (Literal s) =  genYulLit s
genYulExpr (Var s) = genYulVar (Var s)
genYulExpr (FunctionExpr name args expr) = genYulFunctionExpr (FunctionExpr name args expr)
genYulExpr (MapExpr name key) = genYulMappingExpr (MapExpr name key)
genYulExpr (MapAssignExpr name key val) = genYulMappingAssign (MapAssignExpr name key val)
genYulExpr (ListExpr name index) = genYulListExpr (ListExpr name index)
genYulExpr (ListAssignExpr name index val) = genYulListAssign (ListAssignExpr name index val)


genYulBinaryExpr :: Expr -> FafelState String
genYulBinaryExpr (BinaryExpr op e1 e2) = do
  e1' <- genYulExpr e1
  e2' <- genYulExpr e2
  op' <- genYulBinaryOp op
  return $ op' ++ "(" ++ e1' ++ "," ++ e2' ++ ")"


genYulCompareExpr :: Expr -> FafelState String
genYulCompareExpr (CompareExpr op e1 e2) = do
    e1' <- genYulExpr e1
    e2' <- genYulExpr e2
    op' <- genYulCompareOp op
    return $ op' ++ "(" ++ e1' ++ "," ++ e2' ++ ")"

genYulFuncCall :: Expr -> FafelState String
genYulFuncCall (FunctionCall name args) = 
    return $ name ++ "(" ++ (intercalate ", " $ map show args) ++ ")"

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

-- This function generates the YUL code for a list declaration
-- It takes a ListDecl and a SymbolTable as inputs and returns a YUL string
-- and an updated SymbolTable that includes the newly declared list.
storeList :: StateVariable -> FafelState String
storeList (ListDecl name val) = do
    slot <- getSlot
    addToSymbolTable name slot
    return $ "sstore(" ++ show slot ++ ", 0)\n"

-- This function generates the YUL code for a mapping declaration
-- It takes a MapDecl and a SymbolTable as inputs and returns a YUL string
-- and an updated SymbolTable that includes the newly declared mapping.
storeMapping :: StateVariable -> FafelState String
storeMapping (MapDecl name ty) = do
    slot <- getSlot
    addToSymbolTable name slot
    return $ "sstore(" ++ show slot ++ ", 0)\n"


genYulContract :: Contract -> FafelState String
genYulContract (Contract name stateVariables functions) = do
    stateVars <- mapM genYulStateVariable stateVariables
    funcs <- mapM genYulFunction functions
    let contract = "contract " ++ name ++ " {\n" ++ (concat stateVars) ++ (concat funcs) ++ "\n}"
    return contract

-- This function take a function node and maps it to its respective yul code.
genYulFunction :: Function -> FafelState String
genYulFunction (Function name _ _ expr) = do
    expr' <- genYulFunctionExpr expr
    return $ "function " ++ name ++ "(" ++ expr'

-- This function takes the functionExpr node and  maps it to its respective yul code.
genYulFunctionExpr :: Expr -> FafelState String
genYulFunctionExpr (FunctionExpr name args expr) = do
    args' <- mapM genYulExpr args
    expr' <- genYulExpr expr
    return $ (intercalate "," args') ++ ")" ++ " -> result { " ++ "result := " ++ expr' ++ " }\n"



-- This function generates the YUL code for a mapping expression
-- It takes a MapExpr and a SymbolTable as inputs and returns a YUL string.
genYulMappingExpr :: Expr -> FafelState String
genYulMappingExpr (MapExpr name key) = do
    slot <- lookupInSymbolTable name
    key' <- genYulExpr key
    case slot of
        Just slot -> return $ "sload(sha3(" ++ show slot ++ "," ++ key' ++ "))"
        Nothing -> return $ error $ "Error: " ++ name ++ " not found in symbol table."

       

-- This function generates the YUL code for a mapping assignment
-- It takes a MapAssign and a SymbolTable as inputs and returns a YUL string
-- and an updated SymbolTable that reflects the change in the mapping.
genYulMappingAssign :: Expr -> FafelState String
genYulMappingAssign (MapAssignExpr name key val) = do
    slot <- lookupInSymbolTable name
    key' <- genYulExpr key
    val' <- genYulExpr val
    case slot of
        Just slot -> return $ "sstore(sha3(" ++ show slot ++ "," ++ key' ++ ")," ++ val' ++ ")"
        Nothing -> return $ error $ "Error: " ++ name ++ " not found in symbol table."


-- this function should search the symbol table for the storage position of the list
-- then use the appropriate yul code to find the value stored in the list. Yul takes a hash
-- of storage slot where the length is stored and each value is stored consecutively thereafter
genYulListExpr :: Expr -> FafelState String
genYulListExpr (ListExpr name index) = do
    slot <- lookupInSymbolTable name
    case slot of
        Just slot -> return $ "sload(sha3(" ++ show slot ++ ", " ++ show index ++ "))"
        Nothing -> return $ error $ "Error: " ++ name ++ " not found in symbol table."

-- this function searches the symbol table for the name of the list to get the storage position
-- of the list, then outputs the appropriate yul code to store the value at the right index to the list.
genYulListAssign :: Expr -> FafelState String
genYulListAssign (ListAssignExpr name index value) = do
    slot <- lookupInSymbolTable name
    case slot of
        Just slot ->
            let indexExpr = genYulExpr index
                indexHash = show (slot + (read $ show index) * 32)
                valueHash = show value
            in return $ "sstore(" ++ indexHash ++ ", " ++ valueHash ++ ")"
        Nothing -> error $ "Error: " ++ name ++ " not found in symbol table."


