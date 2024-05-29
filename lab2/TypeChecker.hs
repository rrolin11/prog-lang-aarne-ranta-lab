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
typecheck p = do 
    env <- emptyEnv
    return checkProg env p

checkProg :: Env -> Program -> Err ()
checkProg env prog = case prog of
    [] -> fail $ "el programa es vacío"
    defs -> return checkDefs env defs

checkDefs :: Env -> [Def] -> Err ()
checkDefs env defs = case defs of
    [] -> fail $ "la lista de definiciones es vacía"
    def:[] -> checkDef env def
    def:defs -> -- IMPLEMENTAR

checkDef :: Env -> Def -> Err ()
checkDef env def -> case def of 
    DFun type id args stms -> do 
        updateFun env id -- IMPLEMENTAR

inferExp :: Env -> Exp -> Err Type
inferExp env x = case x of
    ETrue -> return Type_bool
    EInt n -> return Type_int
    EId id -> lookupVar env id
    EAdd exp1 exp2 ->
        inferBin [Type_int, Type_double, Type_string] env exp1 exp2

inferBin :: [Type] -> Env -> Exp -> Exp -> Err Type
inferBin types env exp1 exp2 = do
    typ <- inferExp env exp1
    if elem typ types
        then
            checkExp env exp2 typ
        else
            fail $ "tipo incorrecto en la expresión " ++ printTree exp1

checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
    typ2 <- inferExp env exp
    if (typ2 = typ) then
        return ()
    else
        fail $ "el tipo de " ++ printTree exp ++
        "se esperaba " ++ printTree typ ++
        "pero se encontró " ++ printTree typ2

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
