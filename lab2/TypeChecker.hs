{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env
import Data.Either

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck prog = case prog of
    PDefs [] -> fail $ "el programa es vacío"
    PDefs defs -> do 
        let env = emptyEnv
        checkProg env prog

loadFunctionToEnv :: [Def] -> Env -> Err Env
loadFunctionToEnv [] env = Right env
loadFunctionToEnv ((DFun typ id args stms):defs) env = do
        let types = [t | (ADecl t id) <- args]
        case updateFun env id (types, typ) of
            Right env' -> loadFunctionToEnv defs env'
            Left err -> Left err

checkProg :: Env -> Program -> Err ()
checkProg env prog = case prog of
    PDefs [] -> fail $ "el programa es vacío"
    PDefs defs ->
        case loadFunctionToEnv defs env of
            Right env' -> 
                case checkDefs env' defs of
                    Right ret -> Right ret
                    Left err -> Left err
            Left err -> Left err

checkDefs :: Env -> [Def] -> Err ()
checkDefs env defs =
    case defs of
        [] -> return ()
        def:defs -> do
            _ <- checkDef env def
            checkDefs env defs

loadArgsToEnv :: [Arg] -> Env -> Err Env
loadArgsToEnv [] env = Right env
loadArgsToEnv ((ADecl typ id):args) env =
    case updateVar env [id] typ of
        Right env' -> loadArgsToEnv args env'
        Left err -> Left err
        

checkDef :: Env -> Def -> Err ()
checkDef env (DFun typ id args stms) = do
    env' <- loadArgsToEnv args env
    _ <- checkStms typ env' stms
    return ()
    
inferExp :: Env -> Exp -> Err Type
inferExp env x = case x of
    ETrue -> return Type_bool
    EFalse -> return Type_bool
    EInt n -> return Type_int
    EDouble n -> return Type_double
    EString s -> return Type_string
    EId id -> lookupVar env id 
    EApp id exps ->
        case lookupFun env id of
            Right (types, typ) -> do
                let typesExp = rights [inferExp env e | e <- exps]
                if (types == typesExp) then
                        Right typ
                else
                        Left $ "alguno de los tipos no coincide"
            Left err -> Left err       
    EPIncr exp -> inferExp env exp
    EPDecr exp -> inferExp env exp
    EIncr exp -> inferExp env exp
    EDecr exp -> inferExp env exp
    ETimes exp1 exp2 -> inferBin [Type_int, Type_double, Type_string] env exp1 exp2
    EDiv exp1 exp2 -> inferBin [Type_int, Type_double, Type_string] env exp1 exp2
    EPlus exp1 exp2 -> inferBin [Type_int, Type_double, Type_string] env exp1 exp2
    EMinus exp1 exp2 -> inferBin [Type_int, Type_double, Type_string] env exp1 exp2
    ELt exp1 exp2 -> 
        case inferBin [Type_int, Type_double, Type_string] env exp1 exp2 of
            Right _ -> Right Type_bool
            Left err -> Left err
    EGt exp1 exp2 ->
        case inferBin [Type_int, Type_double, Type_string] env exp1 exp2 of
            Right _ -> Right Type_bool
            Left err -> Left err
    ELtEq exp1 exp2 ->
        case inferBin [Type_int, Type_double, Type_string] env exp1 exp2 of
            Right _ -> Right Type_bool
            Left err -> Left err
    EGtEq exp1 exp2 ->
        case inferBin [Type_int, Type_double, Type_string] env exp1 exp2 of
            Right _ -> Right Type_bool
            Left err -> Left err
    EEq exp1 exp2 ->
        case inferBin [Type_int, Type_double, Type_string] env exp1 exp2 of
            Right _ -> Right Type_bool
            Left err -> Left err
    ENEq exp1 exp2 ->
        case inferBin [Type_int, Type_double, Type_string] env exp1 exp2 of
            Right _ -> Right Type_bool
            Left err -> Left err
    EAnd exp1 exp2 ->
        case inferBin [Type_bool] env exp1 exp2 of
            Right _ -> Right Type_bool
            Left err -> Left err
    EOr exp1 exp2 ->
        case inferBin [Type_bool] env exp1 exp2 of
            Right _ -> Right Type_bool
            Left err -> Left err
    EAss exp1 exp2 ->
        case inferBin [Type_int, Type_double, Type_string] env exp1 exp2 of
            Right _ -> inferExp env exp1
            Left err -> Left err
    ETyped exp typ -> Left $ "Fallo del ETyped "


checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp =
    case inferExp env exp of
        Right typ2 -> do
            if (typ2 == typ) then
                Right ()
            else
                Left $ "el tipo de " ++ printTree exp ++
                "se esperaba " ++ printTree typ ++
                "pero se encontró " ++ printTree typ2
        Left err -> Left err

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env exp1 exp2 =
    case inferExp env exp1 of
        Right typ -> do
            if elem typ types
                then
                    case checkExp env typ exp2 of
                        Right _ -> Right typ
                        Left err -> Left err
                else
                    Left $ "tipo incorrecto en la expresión " ++ printTree exp1
        Left err -> Left err

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms typ env stms =
    case stms of
        [] -> Right env
        s:ss -> case checkStm typ env s of
            Right env' -> checkStms typ env ss
            Left err -> Left err

checkStm :: Type -> Env -> Stm -> Err Env
checkStm typ env x = case x of    
    SExp exp ->
        case inferExp env exp of
            Right _ -> Right env
            Left err -> Left err
    SDecls typ ids ->
        case updateVar env ids typ of
            Right env' -> Right env'
            Left err -> Left err
    SInit typ id exp ->
        case updateVar env [id] typ of
            Right env' ->
                case checkExp env typ exp of
                    Right _ -> Right env'
                    Left err -> Left err
            Left err -> Left err
    SReturn exp ->
        case checkExp env typ exp of
            Right _ -> Right env
            Left err -> Left err
    SReturnVoid -> Right env
    SWhile exp stm ->
        case checkExp env Type_bool exp of
            Right _ ->
                case checkStm typ env stm of
                    Right _ -> Right env
                    Left err -> Left err
            Left err -> Left err
    SBlock stms -> do
        let env' = newBlock env
        case checkStms typ env' stms of
            Right _ -> Right env
            Left err -> Left err
    SIfElse exp istm estm ->
        case checkExp env Type_bool exp of
            Right _ ->
                case checkStm typ env istm of
                    Right _ ->
                        case checkStm typ env estm of
                            Right _ -> Right env
                            Left err -> Left err
                    Left err -> Left err
            Left err -> Left err
