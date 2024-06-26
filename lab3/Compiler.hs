module Compiler where

-- java virtual machine instructions reference
-- http://cs.au.dk/~mis/dOvs/jvmspec/ref-Java.html

import AbsCPP
import TypeChecker

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans.State.Lazy
--import Control.Monad.State

type Instruction = String
type FunType     = String
type Label       = Int

data Env = Env {
  funs      :: Map.Map Id FunType,  -- Asocia el identificador de función y su instrucción JVM
  vars      :: [Map.Map Id Int],    -- Stack of variables blocks !
  maxvars   :: [Int],               -- Stack of counter for variable addresses in each block
  maxstk    :: Int,                 -- counter for maximal stack depth
  labels    :: Int,                 -- counter for jump labels
  code      :: [Instruction] 
}

emptyEnv :: Env
emptyEnv = Env { funs=Map.empty,
                 vars=[], -- Mapea el nombre de variable y su dirección de memoria.
                 maxvars=[0],
                 maxstk=0,
                 labels=0,
                 code=[]}

emit :: Instruction -> State Env ()
emit i = modify (\s -> s {code = i : code s})

typeSize :: Type -> Int
typeSize Type_double = 2
typeSize _           = 1

extendVar :: Type -> Id -> State Env ()
extendVar t i = do
  s <- get
  let m:ms = vars s
  let a:as = maxvars s
  put $ s {vars = (Map.insert i a m)}
  put $ s {maxvars = (a + (typeSize t)):as}

extendArg :: Arg -> State Env ()
extendArg (ADecl t i) -> extendVar t i

extendVars :: Type -> [Id] -> State Env ()
extendVars _ [] = error $ "La lista de variables a extender es vacía."
extendVars t ids = mapM_ (extendVar t) ids

typeJVM :: Type -> String
typeJVM Type_int    = "I"
typeJVM Type_double = "D"
typeJVM Type_void   = "V"
typeJVM Type_bool   = "Z"
typeJVM Type_string = "Ljava/lang/String;"

funJVMType :: String -> [Type] -> Type -> FunType
funJVMType i typs rty = i ++ "(" ++ (foldr (\ t s -> typeJVM t ++ s) "" typs) ++ ")" ++ typeJVM rty

-- Retorna la instrucción de JVM equivalente al llamado de una función.
funJVM :: String -> String -> [Type] -> Type -> FunType
funJVM i clas argty rty = "invokestatic " ++ clas ++"/" ++ funJVMType i argty rty

extendFunEnv :: String -> FunType -> State Env ()
extendFunEnv i ft = do
  s <- get
  let m = Map.insert i ft (funs s)
  put $ s {funs = m}

extendDef :: String -> Def -> State Env ()
extendDef cls (DFun t (Id i) args stmts) = do
  extendFunEnv i (funJVM i cls [t | (ADecl t id) <- args] t)

-- Besides added in type checker
extendBuiltinDefs :: State Env ()
extendBuiltinDefs = mapM_ ( \ ((Id i),(argTys,rty)) -> extendFunEnv i $ funJVM i "Runtime" argTys rty) buildInFunctions

newBlock :: State Env ()
newBlock = do
  s <- get
  let v = vars s 
  let m = maxvars s 
  put $ s {vars = (Map.empty):v}
  put $ s {maxvars = (head m):m}

exitBlock :: State Env ()
exitBlock = do
  s <- get
  let v:vs = vars s 
  let m:ms = maxvars s 
  put $ s {vars = vs}
  put $ s {maxvars = ms}

newLabel :: State Env String
newLabel = do
  s <- get
  let lbl = labels s
  put $ s {labels = 1 + lbl}
  return $ "L_" ++ show lbl
  
lookupFun :: Id -> State Env FunType
lookupFun i = do
  s <- get
  return (fromJust Map.lookup i (funs s))

findAddress :: Id -> [Map.Map Id Int] -> Int
findAddress i [] = error $ "No se encontró el identificador de variable " ++ printTree i ++ "."
findAddress i v:vs = case Map.lookup i v of 
                Just address -> return address
                Nothing -> findAddress i vs

lookupVar :: Id -> State Env Int
 lookupVar i = do
 s <- get
 return $ findAddress i (vars s)

-- Entry point from ccpp.
-- Arguments: cls is the class name and p is the typed embedded abstract syntax tree (returned by the type checker).
-- Hints: call compileP and run the State monad !
compile :: String -> Program -> [Instruction]
compile cls p = do 
  execState State emptyEnv ()
  s <- compileP cls p
  return reverse (code s)

compileP :: String -> Program -> State Env () 
compileP cls (PDefs defs) = do
  emit $ ".class public " ++ cls
  emit $ ".super java/lang/Object"
  emit $ ""
  emit $ ".method public <init>()V"
  emit $ "  aload_0"
  emit $ "  invokenonvirtual java/lang/Object/<init>()V"
  emit $ "  return"
  emit $ ".end method"
  emit $ ""
  extendBuiltinDefs
  mapM  (extendDef cls) defs
  mapM_ compileDef defs

compileDef :: Def -> State Env ()
compileDef (DFun t (Id i) args stmts) = do
  newBlock 
  if i == "main" then do
       emit $ ".method public static main([Ljava/lang/String;)V"
       extendVar Type_string (Id "args") -- in fact is an array of strings ([Ljava/lang/String;)
  else emit $ ".method public static " ++ (funJVMType i (map (\ (ADecl t _) -> t) args) t)
  emit $ ".limit locals 1000"
  emit $ ".limit stack  1000"  -- aca se puede ser mas precisoo!
  mapM (\ (ADecl t i) -> extendVar t i) args
  mapM compileStm stmts
  exitBlock
  emit $ "return"
  emit $ ".end method"
  emit ""

compileStm :: Stm -> State Env ()
compileStm stm = undefined

compileInt :: Integer
compileInt -1 = emit $ "iconst_m1"
compileInt 0 = emit $ "iconst_0"
compileInt 1 = emit $ "iconst_1"
compileInt 2 = emit $ "iconst_2"
compileInt 3 = emit $ "iconst_3"
compileInt 4 = emit $ "iconst_4"
compileInt 5 = emit $ "iconst_5"
compileInt i 
 | -256 <= i && i < 256 = emit $ "bipush " ++ i
 otherwise = emit $ "ldc " ++ i

compileDouble :: Double 
compileDouble 0.0 = emit $ "dconst_0"
compileDouble 1.0 = emit $ "dconst_1"
compileDouble d = emit $ "ldc2_w " ++ d

compileExp :: Exp -> State Env ()
compileExp ETyped EFalse _ = emit $ "iconst_0"
compileExp ETyped ETrue _ = emit $ "iconst_1"
compileExp ETyped (EInt i) _ = compileInt i
compileExp ETyped (EDouble d) _ = compileDouble d
compileExp ETyped (EId i) t = do
  let address = lookupVar i
  case t of
    Type_string -> emit $ "aload " ++ address
    Type_double -> emit $ "dload " ++ address
    _ -> emit $ "iload " ++ address
compileExp ETyped (EApp id exps) = do
  emit $ lookupFun id
  mapM_ compileExp exps
compileExp (ETyped (ETimes e1 e2) t) = do
  compileExp e1 
  compileExp e2
  case t of    
    Type_double -> emit $ "dmul"
    _ -> emit $ "imul"
compileExp (ETyped (EDiv e1 e2) t) = do
  compileExp e1 
  compileExp e2
  case t of    
    Type_double -> emit $ "ddiv"
    _ -> emit $ "idiv"
compileExp (ETyped (EPlus e1 e2) t) = do
  compileExp e1 
  compileExp e2
  case t of    
    Type_double -> emit $ "dadd"
    _ -> emit $ "iadd"
compileExp (ETyped (EPlus e1 e2) t) = do
  compileExp e1 
  compileExp e2
  case t of    
    Type_double -> emit $ "dsub"
    _ -> emit $ "isub"
compileExp (EEq e1 e2) = compileCmp Equal e1 e2
  
compileCmp :: Cmp -> Exp -> Exp -> State Env ()
compileCmp c e1 e2 = do 
  labelTrue <- newLabel
  labelEnd <- newLabel
  compileExp e1
  compileExp e2
  emit (show c ++ labelTrue)
  emit $ "iconst_0"
  emit ("goto " ++ labelEnd)
  emit (labelTrue ++ ":")
  emit $ "iconst_1"
  emit (labelEnd ++ ":")

-- Hints: usefull auxiliary functions for comparations compilation
data Cmp = Equal | NEqual | Lt | Gt | Ge | Le
  deriving (Eq)

instance Show Cmp where
  show Equal  = "if_icmpeq "
  show NEqual = "if_icmpne "
  show Lt     = "if_icmplt "
  show Gt     = "if_icmpgt "
  show Ge     = "if_icmpge "
  show Le     = "if_icmple "

showDbl :: Cmp -> Instruction
showDbl Equal  = "ifeq "
showDbl NEqual = "ifne "
showDbl Lt     = "iflt "
showDbl Gt     = "ifgt "
showDbl Ge     = "ifge "
showDbl Le     = "ifle "
