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
typecheck p = fail "no implementado" 

checkProg :: Env -> Program -> Err ()
checkProg env prog = do


checkDef :: Env -> Def -> Err ()
checkDef = undefined

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
            fail $ "wrong type of expression " ++ printTree exp1

checkExp :: Env -> Type -> Exp -> Err ()
checkExp env typ exp = do
    typ2 <- inferExp env exp
    if (typ2 = typ) then
        return ()
    else
        fail $ "type of " ++ printTree exp ++
        "expected " ++ printTree typ ++

checkStms :: Type -> Env -> [Stm] -> Err Env
checkStms = undefined

checkStms :: Env-> [Stm]-> Err Env
checkStms env stms = case stms of
    []-> return env
    x : rest-> do
        env' <- checkStm env x
        checkStms env' rest

checkStm :: Env -> Type -> Stm -> Err Env
checkStm env val x = case x of
    SExp exp -> do
        inferExp env exp
        return env
    SDecl typ x ->
        updateVar env id typ
    SWhile exp stm-> do
        checkExp env Type_bool exp
        checkStm env val stm

checkFun :: Env -> Def -> Err ()