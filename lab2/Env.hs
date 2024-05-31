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
lookupVar (s, []) id = fail $ "el contexto es vacío";
lookupVar (s, ctx:ctxs) id = 
    case Map.lookup id ctx of
        Nothing -> lookupVar (s, ctxs) id
        Just t  -> return t

lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (sig, _) id = 
    case Map.lookup id sig of
        Nothing -> Left $ "la función " ++ printTree id ++ "no se encontró"
        Just fun -> Right fun

updateVar :: Env -> [Id] -> Type -> Err Env
updateVar (_, []) [] _ = Left $ "la lista de identificadores es vacía"
updateVar (s, []) (id:[]) typ = Right (s, [Map.insert id typ empty])
updateVar (s, []) (id:ids) typ = updateVar (s, [Map.insert id typ empty]) ids typ
updateVar (s, ctx:ctxs) (id:[]) typ = 
    case Map.lookup id ctx of
        Nothing -> Right (s, (Map.insert id typ ctx):ctxs)
        Just t -> Left $ "la declaración de variable " ++ printTree id ++ "ya existe en este contexto"
updateVar (s, ctx:ctxs) (id:ids) typ = 
    case Map.lookup id ctx of
        Nothing -> updateVar (s, (Map.insert id typ ctx):ctxs) ids typ
        Just t -> Left $ "la declaración de variable " ++ printTree id ++ "ya existe en este contexto"

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (sig, ctx) id (types, typ) = 
    case Map.lookup id sig of
        Nothing -> Right ((Map.insert id (types, typ) sig), ctx)
        Just (types2, type2) -> do
            if (type2 /= typ && types2 /= types) then
                Right ((Map.insert id (types, typ) sig), ctx)
            else 
                Left $ "la función " ++ printTree id ++ "ya existe en el mapa de firmas"

newBlock :: Env -> Env
newBlock (sig, ctx) = (sig, (empty):ctx)

emptyEnv :: Env
emptyEnv = (empty, [])
