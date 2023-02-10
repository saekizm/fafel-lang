module Codegen where

import Parser
import Data.Map (Map)
import Data.List
import qualified Data.Map as Map


-- Beginning the code generation for Fafel to Yul. 
-- Much more research to be done on Yul constructs and how to map fafel to yul.

type SymbolTable = Map String Int

initialSymbolTable :: SymbolTable
initialSymbolTable = Map.empty

getSlot :: SymbolTable -> (Int, SymbolTable)
getSlot sym = 
  let currentSlot = Map.size sym
  in (currentSlot, Map.insert "next_free_slot" (currentSlot + 1) sym)

addToSymbolTable :: String -> Int -> SymbolTable -> SymbolTable
addToSymbolTable name slot sym = Map.insert name slot sym

lookupInSymbolTable :: String -> SymbolTable -> Maybe Int
lookupInSymbolTable name sym = Map.lookup name sym

genYulLit :: Literal -> String
genYulLit (IntLit s) = show s
genYulLit (FloatLit s) = show s
genYulLit (BoolLit True) = "true"
genYulLit (BoolLit False) = "false"
genYulLit (AddressLit s) = "address(" ++ s ++ ")"

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
genYulExpr (FunctionCall n a) = genYulFuncCall (FunctionCall n a)
genYulExpr (BinaryExpr op e1 e2) = genYulBinaryExpr (BinaryExpr op e1 e2)
genYulExpr (CompareExpr op e1 e2) = genYulCompareExpr (CompareExpr op e1 e2)
genYulExpr (Literal s) =  genYulLit s
genYulExpr (Var s) = genYulVar (Var s)
genYulExpr (FunctionExpr name args expr) = genYulFunctionExpr (FunctionExpr name args expr)


genYulBinaryExpr :: Expr -> String
genYulBinaryExpr (BinaryExpr op e1 e2) = 
    (genYulBinaryOp op) ++ "(" ++ (genYulExpr e1) ++ "," ++ (genYulExpr e2) ++ ")"

genYulCompareExpr :: Expr -> String
genYulCompareExpr (CompareExpr op e1 e2) = 
    genYulCompareOp op ++ "(" ++ genYulExpr e1 ++ "," ++ genYulExpr e2 ++ ")"

genYulFuncCall :: Expr -> String
genYulFuncCall (FunctionCall name args) = 
    name ++ "(" ++ (intercalate ", " $ map show args) ++ ")"

--genYulIfExpr :: Expr -> String

genYulVar :: Expr -> String
genYulVar (Var name) = name

genYulStateVariable :: StateVariable -> SymbolTable -> (String, SymbolTable)
genYulStateVariable (MapDecl name ty) sym =
    storeMapping (MapDecl name ty) sym
genYulStateVariable (ListDecl name ty) sym =
    storeList (ListDecl name ty) sym
genYulStateVariable (IntDecl name ty) sym =
    storeInt (IntDecl name ty) sym 
genYulStateVariable (FloatDecl name ty) sym =
    storeFloat (FloatDecl name ty) sym
genYulStateVariable (BoolDecl name ty) sym =
    storeBool (BoolDecl name ty) sym
genYulStateVariable (AddressDecl name ty) sym =
    storeAddress (AddressDecl name ty) sym

-- this function gets the next slot in storage and stores an int.
storeInt :: StateVariable -> SymbolTable -> (String, SymbolTable)
storeInt (IntDecl name ty) sym =
    let (slot, newsym) = getSlot sym
    in ("sstore(" ++ show slot ++ ", 0)",
    addToSymbolTable name slot newsym) 

-- this function gets the next slot in storage and stores a float.
storeFloat :: StateVariable -> SymbolTable -> (String, SymbolTable)
storeFloat (FloatDecl name ty) sym =
    let (slot, newsym) = getSlot sym
    in ("sstore(" ++ show slot ++ ", 0)",
    addToSymbolTable name slot newsym)

-- this function gets the next slot in storage and stores a bool value.
storeBool :: StateVariable -> SymbolTable -> (String, SymbolTable)
storeBool (BoolDecl name ty) sym =
    let (slot, newsym) = getSlot sym
    in ("sstore(" ++ show slot ++ ", 0",
    addToSymbolTable name slot newsym)

-- this function gets the next slot in storage and stores an address.
storeAddress :: StateVariable -> SymbolTable -> (String, SymbolTable)
storeAddress (AddressDecl name ty) sym =
    let (slot, newsym) = getSlot sym
    in ("sstore(" ++ show slot ++ ", 0",
    addToSymbolTable name slot newsym)

-- This function generates the YUL code for a list declaration
-- It takes a ListDecl and a SymbolTable as inputs and returns a YUL string
-- and an updated SymbolTable that includes the newly declared list.
storeList :: StateVariable -> SymbolTable -> (String, SymbolTable)
storeList (ListDecl name ty) sym = 
    -- Get the next available storage slot from the symbol table
    let (currentSlot, newsym) = getSlot sym
        -- Add the list name and its storage slot to the symbol table
        newsym' = addToSymbolTable name currentSlot newsym
    -- Return the YUL code for list declaration and the updated symbol table
    in ("sstore(" ++ show currentSlot ++ ", 0)", newsym')

-- This function generates the YUL code for a mapping declaration
-- It takes a MapDecl and a SymbolTable as inputs and returns a YUL string
-- and an updated SymbolTable that includes the newly declared mapping.
storeMapping :: StateVariable -> SymbolTable -> (String, SymbolTable)
storeMapping (MapDecl name ty) sym = 
    -- Get the next available storage slot from the symbol table
    let (currentSlot, newsym) = getSlot sym
        -- Add the mapping name and its storage slot to the symbol table
        newsym' = addToSymbolTable name currentSlot newsym
    -- Return the YUL code for mapping declaration and the updated symbol table
    in ("sstore(" ++ show currentSlot ++ ", 0)", newsym')


genYulContract :: Contract -> String
genYulContract (Contract name stateVariables functions) =
  let stateVariablesCode = concatMap (\x -> fst (genYulStateVariable x initialSymbolTable)) stateVariables
      functionsCode = concatMap genYulFunction functions
  in "contract " ++ name ++ " {\n" ++ stateVariablesCode ++ functionsCode ++ "}"


-- This function take a function node and maps it to its respective yul code.
genYulFunction :: Function -> String
genYulFunction (Function name args return expr) =
    "function" ++ name ++ "(" ++ (genYulFunctionExpr expr)

-- This function takes the functionExpr node and  maps it to its respective yul code.
genYulFunctionExpr :: Expr -> String
genYulFunctionExpr (FunctionExpr name args expr) =
    (intercalate "," $ map genYulExpr args) ++ ")" ++ " -> result { " ++ genYulExpr expr ++ " }"


-- This function generates the YUL code for a mapping expression
-- It takes a MapExpr and a SymbolTable as inputs and returns a YUL string.
genYulMappingExpr :: Expr -> SymbolTable -> String
genYulMappingExpr (MapExpr name key) sym =
  -- Lookup the storage slot of the mapping in the symbol table
  case lookupInSymbolTable name sym of
    -- If found, generate the YUL code to get the value stored in the mapping
    Just slot -> "sload(sha3(" ++ show slot ++ "," ++ (genYulExpr key) ++ "))"
    -- If not found, raise an error
    Nothing -> error $ "Error: " ++ name ++ " not found in symbol table."

-- This function generates the YUL code for a mapping assignment
-- It takes a MapAssign and a SymbolTable as inputs and returns a YUL string
-- and an updated SymbolTable that reflects the change in the mapping.
genYulMappingAssign :: Expr -> SymbolTable -> String
genYulMappingAssign (MapAssignExpr name key val) sym =
  case lookupInSymbolTable name sym of
    Just slot -> "sstore(sha3(" ++ show slot ++ "," ++ genYulExpr key ++ ")," ++ genYulExpr val ++ ")"
    Nothing -> error $ "Error: " ++ name ++ " not found in symbol table."


-- this function should search the symbol table for the storage position of the list
-- then use the appropriate yul code to find the value stored in the list. Yul takes a hash
-- of storage slot where the length is stored and each value is stored consecutively thereafter
genYulListExpr :: Expr -> SymbolTable -> String
genYulListExpr (ListExpr name index) sym =
  case lookupInSymbolTable name sym of
    Just slot -> "sload(sha3(" ++ show slot ++ ", " ++ show index ++ "))"
    Nothing -> error $ "Error: " ++ name ++ " not found in symbol table."

-- this function searches the symbol table for the name of the list to get the storage position
-- of the list, then outputs the appropriate yul code to store the value at the right index to the list.
genYulListAssign :: Expr -> SymbolTable -> String
genYulListAssign (ListAssignExpr name index value) sym =
  case lookupInSymbolTable name sym of
    Just slot ->
      let indexExpr = genYulExpr index
          indexHash = show (slot + (read $ genYulExpr index) * 32)
          valueHash = genYulExpr value
      in "sstore(" ++ indexHash ++ ", " ++ valueHash ++ ")"
    Nothing -> error $ "Error: " ++ name ++ " not found in symbol table."


