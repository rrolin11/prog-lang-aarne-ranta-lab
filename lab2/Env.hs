{-# LANGUAGE CPP #-}

module Env where

import AbsCPP
import PrintCPP
import ErrM

import Data.Map as Map

#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 808
import Prelude hiding (fail)
import Control.Monad hiding (fail)

fail = Bad
#endif

type Env = (Sig, [Context])
type Sig = Map Id ([Type],Type)
type Context = Map Id Type

lookupVar :: Env -> Id -> Err Type
lookupVar (s, []) id = fail $ "La variable \"" ++ printTree id ++ "\" no ha sido declarada en este ámbito.";
lookupVar (s, ctx:ctxs) id = 
    case Map.lookup id ctx of
        Nothing -> lookupVar (s, ctxs) id
        Just t  -> return t

lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (sig, _) id = 
    case Map.lookup id sig of
        Nothing -> Left $ "La función \"" ++ printTree id ++ "\" no ha sido declarada."
        Just fun -> Right fun

updateVars :: Env -> [Id] -> Type -> Err Env
updateVars env [] typ = Right env
updateVars env (id:ids) typ = 
    case updateVar env id typ of
        Right env' -> updateVars env' ids typ
        Left err -> Left err

{-
Agrega una declaración de variable al contexto tope del stack. 
Si ya existe, lanza un error. De otro modo, devuelve el entorno 
actualizado con la nueva declaración.
-} 
updateVar :: Env -> Id -> Type -> Err Env
updateVar (sig, []) id typ = Right (sig, [Map.insert id typ empty])
updateVar (sig, ctx:ctxs) id typ = 
    case Map.lookup id ctx of
        Nothing -> Right (sig, (Map.insert id typ ctx):ctxs)
        Just v -> Left $ "La variable \"" ++ printTree typ ++ 
                    " " ++ printTree id ++ "\" ya fue declarada en el scope."

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sig, ctx) id (types, typ) = 
    case Map.lookup id sig of
        Nothing -> Right ((Map.insert id (types, typ) sig), ctx)
        Just (types2, type2) -> do
            if (type2 == typ && types2 /= types) then
                Right ((Map.insert id (types, typ) sig), ctx)
            else 
                Left $ "La función \"" ++ printTree typ ++ 
                    " " ++ printTree id ++ "\" ha sido duplicada al declarar."

newBlock :: Env -> Env
newBlock (sig, ctx) = (sig, (empty):ctx)

emptyEnv :: Env
emptyEnv = (empty, [])
