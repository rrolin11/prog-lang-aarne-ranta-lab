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

type Env = (Sig,[Context])
type Sig = Map Id ([Type],Type)
type Context = Map Id Type

lookupVar :: Env -> Id -> Err Type
lookupVar (_, []) id = fail $ "el contexto es vacío" ++ printTree id ++ "."
lookupVar (sig, ctx:ctxs) id = case Map.lookup id ctx of
                    Nothing   -> lookupVar (sig, ctxs) id
                    Just type -> return type

lookupFun :: Env -> Id -> Err ([Type], Type)
lookupFun (Map.empty, []) _ = fail $ "el entorno es vacío" ++ printTree id ++ "."
lookupFun (sig, ctx:ctxs) id = case Map.lookup id sig of
                    Nothing -> fail $ "la función no se encontró" ++ printTree id ++ "."
                    Just fun -> return fun

updateVar :: Env -> [Id] -> Type -> Err Env
updateVar (_, []) [] type = fail $ "la declaración ya existe en este contexto"
updateVar (_, []) id:[] type = return (_, [Map.insert id type []])
updateVar (_, []) id:ids type = updateVar (_, [Map.insert id type []]) ids type
updateVar (_, ctx:ctxs) id:[] type = case Map.lookup id ctx of
                    Nothing -> return (_, (Map.insert id type ctx):ctxs)
                    Just type -> fail $ "la declaración ya existe en este contexto" ++ 
                    printTree x ++ "."    
updateVar (_, ctx:ctxs) id:ids type = case Map.lookup id ctx of
                    Nothing -> updateVar (_, [Map.insert id type ctx]:ctxs) ids type
                    Just type -> fail $ "la declaración ya existe en este contexto" ++ 
                    printTree x ++ "."    

updateFun :: Env -> Id -> ([Type],Type) -> Err Env
updateFun (Map.empty, ctx) id (types, type) = return ((Map.insert id (types, type) Map.empty), ctx)
updateFun (sig, ctx) id (types, type) = case Map.lookup id sig of
                    Nothing -> return ((Map.insert id (types, type) sig), ctx)
                    Just (types2, type2) -> do
                        if (type2 != type && types2 != types) then
                            return ((Map.insert id (types, type) sig), ctx)
                        else 
                            fail $ "la función ya existe en el mapa de firmas" ++ 
                            printTree id ++ "."   

newBlock :: Env -> Env
newBlock (sig, ctx) -> return (sig, []:ctx)

emptyEnv :: Env
emptyEnv = (Map.empty, [])
