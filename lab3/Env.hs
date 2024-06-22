module Env where

import AbsCPP
import PrintCPP
import ErrM

import qualified Data.Map.Strict as Map

import Prelude hiding (fail)
import Control.Monad hiding (fail)
import Control.Monad.Fail 

type Env = (Sig,[Context])
type Sig = Map.Map Id ([Type],Type)
type Context = Map.Map Id Type

lookupVar :: (MonadFail m) =>  Env -> Id        -> m Type
lookupVar (s,[])   i = fail $ "Undefined variable:" ++ show i
lookupVar (s,c:cs) i = maybe (lookupVar (s,cs) i) return (Map.lookup i c)

lookupFun :: (MonadFail m) => Env -> Id                  -> m ([Type],Type)
lookupFun (s,c)    i 
  = maybe (fail  $ "Undefined function:" ++ show i) return (Map.lookup i s)

updateVar :: (MonadFail m) => Env -> Id -> Type          -> m Env
updateVar (s,c:cs) i t =
	  case Map.member i c of {
	       True  -> fail ("Variable redefinition:" ++ show i);
	       False -> return (s, (Map.insert i t c):cs)
	  }

updateVars :: (MonadFail m) => Env -> [(Id,Type)]        -> m Env
updateVars env = foldl (\ r (i,t) -> r >>= \ e -> updateVar e i t) (return env)

updateFun :: (MonadFail m) => Env -> Id -> ([Type],Type) -> m Env
updateFun (s,c) i sig =
	  case Map.member i s of {
	       True  -> fail ("Function redefinition:" ++ show i);
	       False -> return (Map.insert i sig s,c)
	  }

updateFuns :: (MonadFail m) => [(Id , ([Type],Type))] -> m Env
updateFuns = foldl (\ r (i, args) -> r >>= \ e -> updateFun e i args) (return emptyEnv)

newBlock  :: Env -> Env
newBlock (s,cs) = (s,Map.empty:cs)

emptyEnv  :: Env
emptyEnv = (Map.empty,[Map.empty])
