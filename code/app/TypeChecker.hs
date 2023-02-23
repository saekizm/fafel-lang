module TypeChecker where

import AST
import Data.Map as Map (Map, empty, insert, lookup, delete)
import Data.Either
import Control.Monad.State

type Result = Either String DataType
type Symbol = Either DataType (Map.Map String DataType)
type SymbolTable = Map.Map String Symbol
type FafelState a = StateT SymbolTable IO a

-- our symbol table is a map from function names to a map of variable names to types or to a single type

-- add each state variable to the SymbolTable as a single DataType
insertStateVars :: [StateVariable] -> FafelState ()
insertStateVars [] = return ()
insertStateVars (x:xs) = do
    case x of
        IntDecl name ty -> do
            modify (Map.insert name (Left ty))
            insertStateVars xs
        FloatDecl name ty -> do
            modify (Map.insert name (Left ty))
            insertStateVars xs
        BoolDecl name ty -> do
            modify (Map.insert name (Left ty))
            insertStateVars xs
        AddressDecl name ty -> do
            modify (Map.insert name (Left ty))
            insertStateVars xs
        ListDecl name ty -> do
            modify (Map.insert name (Left ty))
            insertStateVars xs
        MapDecl name ty -> do
            modify (Map.insert name (Left ty))
            insertStateVars xs

-- we add all the info to the symbol table as a map
-- we store all the variables in the map, matched with their types
insertFunctions :: [Function] -> FafelState ()
insertFunctions [] = return ()
insertFunctions (x:xs) = do
    case x of
        Function name args ret body -> do
            modify (Map.insert name (Right (Map.empty)))
            insertFunctions xs

insertToFuncMap :: String -> String -> DataType -> FafelState ()
insertToFuncMap funcName name ty = do
    sym <- get
    case Map.lookup funcName sym of
        Just (Right argMap) -> do
            let newMap = Map.insert name ty argMap
            modify (Map.insert name (Right newMap))
        _ -> return ()

typecheckFunction :: Function -> FafelState Result
-- look up the function in the symbol table
-- extract the arg vars from the body
-- add the args and argtypes to the nested map for that function
-- typecheck the body
typecheckFunction (Function name argTypes ret body) = do
    sym <- get
    case body of
        FunctionExpr name args body -> do
            let argNames = map (\(Var v ) -> v) args
            let argNameTypes = zip3 (repeat name) argNames argTypes
            -- for each element in argNameTypes, call insertToFuncMap with the function name, the arg name, and the arg type
            -- then typecheck the body
            mapM_ (\(fname, name, ty) -> insertToFuncMap fname name ty) argNameTypes
            res <- typecheckExpr body
            if res == Right ret
                then return (Right ret)
                else return (Left $ name ++ " function body does not match return type")

typecheckExpr :: Expr -> FafelState Result
typecheckExpr (FunctionCall name args) = do
    sym <- get
    case Map.lookup name sym of
        Just (Right argMap) -> do
            case Map.lookup name argMap of
                Just ty -> return (Right ty)
                Nothing -> return (Left "Function not found")
        Nothing -> return (Left "Function not found")
typecheckExpr (FunctionExpr name args body) = typecheckExpr body
typecheckExpr (BinaryExpr op e1 e2) = do
    case op of
        StateVarAssign -> do
            return (Right StateType)
        Plus -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right IntType)
                (Right FloatType, Right FloatType) -> return (Right FloatType)
                (Right IntType, Right FloatType) -> return (Right FloatType)
                (Right FloatType, Right IntType) -> return (Right FloatType)
                _ -> return (Left "Type error")
        Minus -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right IntType)
                (Right FloatType, Right FloatType) -> return (Right FloatType)
                (Right IntType, Right FloatType) -> return (Right FloatType)
                (Right FloatType, Right IntType) -> return (Right FloatType)
                _ -> return (Left "Type error")
        Times -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right IntType)
                (Right FloatType, Right FloatType) -> return (Right FloatType)
                (Right IntType, Right FloatType) -> return (Right FloatType)
                (Right FloatType, Right IntType) -> return (Right FloatType)
                _ -> return (Left "Type error")
        Divide -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right IntType)
                (Right FloatType, Right FloatType) -> return (Right FloatType)
                (Right IntType, Right FloatType) -> return (Right FloatType)
                (Right FloatType, Right IntType) -> return (Right FloatType)
                _ -> return (Left "Type error")
        And -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right BoolType, Right BoolType) -> return (Right BoolType)
                _ -> return (Left "Type error")
        Or -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right BoolType, Right BoolType) -> return (Right BoolType)
                _ -> return (Left "Type error")
        Equal -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right BoolType)
                (Right FloatType, Right FloatType) -> return (Right BoolType)
                (Right BoolType, Right BoolType) -> return (Right BoolType)
                (Right AddressType, Right AddressType) -> return (Right BoolType)
                _ -> return (Left "Type error")
        NotEqual -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right BoolType)
                (Right FloatType, Right FloatType) -> return (Right BoolType)
                (Right BoolType, Right BoolType) -> return (Right BoolType)
                (Right AddressType, Right AddressType) -> return (Right BoolType)
                _ -> return (Left "Type error")
        Less -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right BoolType)
                (Right FloatType, Right FloatType) -> return (Right BoolType)
                _ -> return (Left "Type error")
        LessEq -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right BoolType)
                (Right FloatType, Right FloatType) -> return (Right BoolType)
                _ -> return (Left "Type error")
        Greater -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right BoolType)
                (Right FloatType, Right FloatType) -> return (Right BoolType)
                _ -> return (Left "Type error")
        GreaterEq -> do
            ty1 <- typecheckExpr e1
            ty2 <- typecheckExpr e2
            case (ty1, ty2) of
                (Right IntType, Right IntType) -> return (Right BoolType)
                (Right FloatType, Right FloatType) -> return (Right BoolType)
                _ -> return (Left "Type error")
typecheckExpr (MapAssignExpr mapName key value) = return $ Right StateType
typecheckExpr (ListAssignExpr listName index value) = return $ Right StateType
typecheckExpr (Var name) = do
    sym <- get
    case Map.lookup name sym of
        Just (Left ty) -> return (Right ty)
        Just (Right argMap) -> do
            case Map.lookup name argMap of
                Just ty -> return (Right ty)
                Nothing -> return (Left "Function not found")
        Nothing -> return (Left "Variable not found")
typecheckExpr (MapExpr name key) = do
    sym <- get
    case Map.lookup name sym of
        Just (Left ty) -> do
            case ty of
                MapType (keyType, valueType) -> return (Right valueType)
                _ -> return (Left "No MapType error")
        Nothing -> return (Left "Variable not found")
typecheckExpr (ListExpr name index) = do
    sym <- get
    case Map.lookup name sym of
        Just (Left ty) -> do
            case ty of
                ListType valueType -> return (Right valueType)
                _ -> return (Left "No ListType error")
        Nothing -> return (Left "Variable not found")
typecheckExpr (IfExpr condExpr thenExpr elseExpr) = do
    condType <- typecheckExpr condExpr
    thenType <- typecheckExpr thenExpr
    elseType <- typecheckExpr elseExpr
    case (condType, thenType, elseType) of
        (Right BoolType, Right thenType, Right elseType) -> do
            if thenType == elseType
                then return (Right thenType)
                else return (Left "then and else type error")
        _ -> return (Left "If expresssion type error")
typecheckExpr (Literal n) = typecheckLiteral n

typecheckLiteral :: Literal -> FafelState Result
typecheckLiteral (IntLit _) = return (Right IntType)
typecheckLiteral (FloatLit _) = return (Right FloatType)
typecheckLiteral (BoolLit _) = return (Right BoolType)
typecheckLiteral (AddressLit _) = return (Right AddressType)

typecheckContract :: Contract -> FafelState String
typecheckContract (Contract name vars funcs) = do
    insertStateVars vars
    insertFunctions funcs
    functionChecks <- mapM typecheckFunction funcs
    if all isRight functionChecks
        then return $ "True"
        -- return a string of all the errors which are lefts
        else return $ (show $ lefts functionChecks)