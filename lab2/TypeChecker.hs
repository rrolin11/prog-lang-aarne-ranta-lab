{-# LANGUAGE CPP #-}

module TypeChecker where

import AbsCPP
import PrintCPP
import ErrM
import Env

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
#endif

typecheck :: Program -> Err ()
typecheck prog = case prog of
    [] -> fail $ "el programa es vacío"
    defs -> do 
        env <- emptyEnv
        return checkProg env prog

loadFunctionToEnv :: [Def] -> Env -> Env
loadFunctionToEnv [] env = env
loadFunctionToEnv (DFun type id args stms):defs env = do
        types <- [t | (ADecl t id) <- args]
        env' <- updateFun env id (types,type)
        loadFunctionToEnv defs env'

checkProg :: Env -> Program -> Err ()
checkProg env prog = case prog of
    [] -> fail $ "el programa es vacío"
    defs -> do 
        env <- loadFunctionToEnv defs env
        checkDefs env defs

checkDefs :: Env -> [Def] -> Err ()
checkDefs env defs = case defs of
    env [] -> ()
    env def:defs -> do
        _ <- checkDef env def
        checkDefs env defs

loadArgsToEnv :: [Arg] -> Env -> Env
loadArgsToEnv [] env = env
loadArgsToEnv (ADecl type id):args env = do
        env' <- updateVar env [id] type
        loadArgsToEnv args env'

checkDef :: Env -> Def -> Err ()
checkDef env (DFun type id args stms) = do
    env' <- loadArgsToEnv args env
    _ <- checkStms type env' stms
    return ()
    
inferExp :: Env -> Exp -> Err Type
inferExp env x = case x of
    ETrue -> return Type_bool
    EFalse -> return Type_bool
    EInt n -> return Type_int
    EDouble n -> return Type_double
    EString s -> return Type_string
    EId id -> lookupVar env id 
    EApp id exps -> do
       (types,type) <- lookupFun env id
       typesExp <- [inferExp env e | e <- exps]
       if (types == typesExp) then
            return type
       else
            fail $ "alguno de los tipos no coincide"   
    EPIncr exp -> inferExp env exp
    EPDecr exp -> inferExp env exp
    EIncr exp -> inferExp env exp
    EDecr exp -> inferExp env exp
    ETimes exp1 exp2 -> inferBin [Type_int, Type_double, Type_string] env exp1 exp2
    EDiv exp1 exp2 -> inferBin [Type_int, Type_double, Type_string] env exp1 exp2
    EPlus exp1 exp2 -> inferBin [Type_int, Type_double, Type_string] env exp1 exp2
    EMinus exp1 exp2 -> inferBin [Type_int, Type_double, Type_string] env exp1 exp2
    ELt exp1 exp2 -> do
        _ <- inferBin [Type_int, Type_double, Type_string] env exp1 exp2
        return Type_bool
    EGt exp1 exp2 -> do
        _ <- inferBin [Type_int, Type_double, Type_string] env exp1 exp2
        return Type_bool
    ELtEq exp1 exp2 -> do
        _ <- inferBin [Type_int, Type_double, Type_string] env exp1 exp2
        return Type_bool
    EGtEq exp1 exp2 -> do
        _ <- inferBin [Type_int, Type_double, Type_string] env exp1 exp2
        return Type_bool
    EEq exp1 exp2 -> do
        _ <- inferBin [Type_int, Type_double, Type_string] env exp1 exp2
        return Type_bool
    ENEq exp1 exp2 -> do
        _ <- inferBin [Type_int, Type_double, Type_string] env exp1 exp2
        return Type_bool
    EAnd exp1 exp2 -> do
        _ <- inferBin [Type_bool] env exp1 exp2
        return Type_bool
    EOr exp1 exp2 -> do
        _ <- inferBin [Type_bool] env exp1 exp2
        return Type_bool
    EAss exp1 exp2 -> do
        _ <- inferBin [Type_int, Type_double, Type_string] env exp1 exp2
        inferExp env exp1
    ETyped exp type -> fail $ "Fallo del ETyped "


checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
    typ2 <- inferExp env exp
    if (typ2 = typ) then
        return ()
    else
        fail $ "el tipo de " ++ printTree exp ++
        "se esperaba " ++ printTree typ ++
        "pero se encontró " ++ printTree typ2

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env exp1 exp2 = do
    typ <- inferExp env exp1
    if elem typ types
        then
            checkExp env exp2 typ
        else
            fail $ "tipo incorrecto en la expresión " ++ printTree exp1

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms type env stms = case stms of
    [] -> return env
    x:rest -> do
        env' <- checkStm type env x
        checkStms env' rest

checkStm :: Type -> Env -> Stm -> Err Env
checkStm type env x = case x of
    SExp exp -> do
        inferExp env exp
        return env
    SDecls typ x ->
        updateVar env id typ
        return env
    SInit typ id exp -> do
        updateVar env id typ
        checkExp env typ exp
        return env
    SReturn exp -> do
        checkExp env type exp
        return env
    SReturnVoid -> do 
        checkExp env Type_void exp
        return env
    SWhile exp stm -> do
        checkExp env Type_bool exp
        checkStm env type stm
        return env
    SBlock stms -> do
        env' <- newBlock env
        checkStms type env' stms
        return env
    SIfElse exp stm estm -> do 
        checkExp env Type_bool Exp
        checkStm type env stm
        checkStm type env estm
        return env
