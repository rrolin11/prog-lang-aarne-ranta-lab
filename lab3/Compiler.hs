module Compiler where

-- java virtual machine instructions reference
-- http://cs.au.dk/~mis/dOvs/jvmspec/ref-Java.html

import AbsCPP
import TypeChecker

import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Trans.State.Lazy
import Data.Maybe
--import Control.Monad.State
import Debug.Trace

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
                 vars=[Map.empty], -- Mapea el nombre de variable y su dirección de memoria.
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
  put $ s {vars = (Map.insert i a m):ms, maxvars = (a + (typeSize t)):as}

extendArg :: Arg -> State Env ()
extendArg (ADecl t i) = extendVar t i

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
  let m = Map.insert (Id i) ft (funs s)
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
  put $ s { vars = (Map.empty):(vars s), maxvars = (head (maxvars s)):(maxvars s) }

exitBlock :: State Env ()
exitBlock = do
  s <- get
  let v:vs = vars s 
  let m:ms = maxvars s 
  put $ s {vars = vs, maxvars = ms}

newLabel :: State Env String
newLabel = do
  s <- get
  let lbl = labels s
  put $ s {labels = 1 + lbl}
  return $ "LBL" ++ show lbl
  
lookupFun :: Id -> State Env FunType
lookupFun i = do
  s <- get
  return $ fromJust $ Map.lookup i (funs s)

findAddress :: Id -> [Map.Map Id Int] -> Int
findAddress i [] = error $ "No se encontró el identificador de variable."
findAddress i (v:vs) = case Map.lookup i v of 
                Just address -> address
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
  let s = execState (compileP cls p) emptyEnv
  let instructions = reverse (code s)
  traceM ("DEBUG [checkDefs]: " ++ show instructions)
  instructions

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
  s <- get
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


-- Compilador de sentencias
compileStm :: Stm -> State Env ()
compileStm (SExp e) = compileStmExp e
compileStm (SDecls t ids) = do
  extendVars t ids
compileStm (SInit t id exp) = do
  s <- get
  let address = head (maxvars s)
  extendVar t id
  compileExp exp
  compileStoreVar t address
compileStm (SReturn (ETyped e t)) = do
  compileExp (ETyped e t)
  case t of
    Type_double -> do
      emit $ "dreturn"
    Type_string -> do
      emit $ "areturn"
    _ -> do
      emit $ "ireturn"
compileStm (SReturnVoid) = do
  emit $ "return"
compileStm (SWhile e s) = do
  labelEnd <- newLabel
  labelLoop <- newLabel
  emit (labelLoop ++ ":")
  compileExp e
  emit ("ifeq " ++ labelEnd)
  compileStm s
  emit ("goto " ++ labelLoop)
  emit (labelEnd ++ ":")
compileStm (SIfElse e s1 s2) = do
  labelFalse <- newLabel
  labelEnd <- newLabel
  compileExp e
  emit ("ifeq " ++ labelFalse)
  compileStm s1
  emit ("goto " ++ labelEnd)
  emit (labelFalse ++ ":")
  compileStm s2
  emit (labelEnd ++ ":")  
compileStm (SBlock sms) = do
  newBlock
  mapM_ compileStm sms
  exitBlock

-- Compilador de expresiones
compileExp :: Exp -> State Env ()
compileExp (ETyped EFalse t) = emit $ "iconst_0"
compileExp (ETyped ETrue t) = emit $ "iconst_1"
compileExp (ETyped (EInt i) t) = compileInt i
compileExp (ETyped (EDouble d) t) = compileDouble d
compileExp (ETyped (EString s) t) = emit $ "ldc " ++ show s
compileExp (ETyped (EId i) t) = do
  address <- lookupVar i  
  compileLoadVar t address
compileExp (ETyped (EApp id exps) t) = do
  mapM_ compileExp exps
  ft <- lookupFun id
  emit $ ft
compileExp (ETyped (EPIncr i) t) = do -- i++
  address <- lookupVar i
  compileLoadVar t address
  compileDup t
  case t of
    Type_double -> compileDouble 1.0
    _ -> compileInt 1
  compileAlg t Plus
  compileStoreVar t address
compileExp (ETyped (EPDecr i) t) = do
  address <- lookupVar i
  compileLoadVar t address
  compileDup t
  case t of
    Type_double -> compileDouble 1.0
    _ -> compileInt 1
  compileAlg t Minus
  compileStoreVar t address
compileExp (ETyped (EIncr i) t) = do -- ++i
  address <- lookupVar i
  compileLoadVar t address
  case t of
    Type_double -> compileDouble 1.0
    _ -> compileInt 1
  compileAlg t Plus
  compileDup t
  compileStoreVar t address
compileExp (ETyped (EDecr i) t) = do
  address <- lookupVar i
  compileLoadVar t address
  case t of
    Type_double -> compileDouble 1.0
    _ -> compileInt 1
  compileAlg t Minus
  compileDup t
  compileStoreVar t address
compileExp (ETyped (ETimes e1 e2) t) = do
  compileExp e1 
  compileExp e2
  compileAlg t Times
compileExp (ETyped (EDiv e1 e2) t) = do
  compileExp e1 
  compileExp e2
  compileAlg t Div
compileExp (ETyped (EPlus e1 e2) t) = do
  compileExp e1 
  compileExp e2
  compileAlg t Plus
compileExp (ETyped (EMinus e1 e2) t) = do
  compileExp e1 
  compileExp e2
  compileAlg t Minus
compileExp (ETyped (ELt e1 e2) t) = compileCmp Lt e1 e2
compileExp (ETyped (EGt e1 e2) t) = compileCmp Gt e1 e2
compileExp (ETyped (ELtEq e1 e2) t) = compileCmp Le e1 e2
compileExp (ETyped (EGtEq e1 e2) t) = compileCmp Ge e1 e2
compileExp (ETyped (EEq e1 e2) t) = compileCmp Equal e1 e2
compileExp (ETyped (ENEq e1 e2) t) = compileCmp NEqual e1 e2
compileExp (ETyped (EAnd e1 e2) t) = compileBool (EAnd e1 e2)
compileExp (ETyped (EOr e1 e2) t) = compileBool (EOr e1 e2)
compileExp (ETyped (EAss i e2) t) = do
  compileExp e2
  address <- lookupVar i
  compileDup t
  compileStoreVar t address
compileExp _ = return ()


--[[[ FUNCIONES AUXILIARES ]]]

-- Hints: usefull auxiliary functions for comparations compilation
data Cmp = Equal | NEqual | Lt | Gt | Ge | Le
  deriving (Eq)

data Alg = Times | Div | Plus | Minus
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

-- Operaciones algebráicas no-dobles
showIntAlg :: Alg -> Instruction
showIntAlg Times = "imul"
showIntAlg Div = "idiv"
showIntAlg Plus = "iadd"
showIntAlg Minus = "isub"

-- Operaciones algebráicas dobles
showDblAlg :: Alg -> Instruction
showDblAlg Times = "dmul"
showDblAlg Div = "ddiv"
showDblAlg Plus = "dadd"
showDblAlg Minus = "dsub"

-- Compila las sentencias de tipo exp. Las que son del estilo "1 + 1;" son ignoradas.
compileStmExp :: Exp -> State Env ()
compileStmExp (ETyped (EApp i e) t) = compileExp (ETyped (EApp i e) t)
compileStmExp (ETyped e t) = do 
  compileExp (ETyped e t)
  compilePop t

compileAlg :: Type -> Alg -> State Env ()
compileAlg Type_string Plus = do
  ft <- lookupFun (Id "concatStr")
  emit $ ft
compileAlg Type_double e = emit (showDblAlg e)
compileAlg _ e = emit (showIntAlg e)

compilePop :: Type -> State Env ()
compilePop t = case t of
    Type_double -> do
      emit $ "pop2"
    _ -> do
      emit $ "pop"

compileDup :: Type -> State Env ()
compileDup t = case t of
    Type_double -> do
      emit $ "dup2"
    _ -> do
      emit $ "dup"

compileStoreVar :: Type -> Int -> State Env ()
compileStoreVar t address = case t of
    Type_double -> do
      emit $ "dstore " ++ show address
    Type_string -> do
      emit $ "astore " ++ show address
    _ -> do
      emit $ "istore " ++ show address

compileLoadVar :: Type -> Int -> State Env ()
compileLoadVar t address = case t of
    Type_string -> emit $ "aload " ++ show address
    Type_double -> emit $ "dload " ++ show address
    _ -> emit $ "iload " ++ show address

-- Emite el código para pushear un entero al stack
compileInt :: Integer -> State Env ()
compileInt -1 = emit $ "iconst_m1"
compileInt 0 = emit $ "iconst_0"
compileInt 1 = do  
  traceM ("DEBUG [compileInt]: Compilando iconst_1")
  emit $ "iconst_1"
compileInt 2 = emit $ "iconst_2"
compileInt 3 = emit $ "iconst_3"
compileInt 4 = emit $ "iconst_4"
compileInt 5 = emit $ "iconst_5"
compileInt i 
 | -256 <= i && i < 256 = emit $ "bipush " ++ show i
 | otherwise = emit $ "ldc " ++ show i

-- Emite el código de push double al stack
compileDouble :: Double -> State Env ()
compileDouble 0.0 = emit $ "dconst_0"
compileDouble 1.0 = emit $ "dconst_1"
compileDouble d = emit $ "ldc2_w " ++ show d

-- Emite el código para compilar comparaciones
compileCmp :: Cmp -> Exp -> Exp -> State Env ()
compileCmp c (ETyped e Type_double) e2 = do 
  labelTrue <- newLabel
  labelEnd <- newLabel
  compileExp (ETyped e Type_double)
  compileExp e2
  emit $ "dcmpg"
  emit (showDbl c ++ labelTrue)
  emit $ "iconst_0"
  emit ("goto " ++ labelEnd)
  emit (labelTrue ++ ":")
  emit $ "iconst_1"
  emit (labelEnd ++ ":")
compileCmp c e1 e2 = do -- TODO: Mejorar lógica de comparación para usar solo un jump.
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

-- Emite el código de compilar conjunciones y disyunciones.
compileBool :: Exp -> State Env ()
compileBool (EAnd e1 e2) = do
  labelFalse <- newLabel
  labelEnd <- newLabel
  compileExp e1
  emit ("ifeq " ++ labelFalse)
  compileExp e2
  emit ("goto " ++ labelEnd)
  emit (labelFalse ++ ":")
  emit $ "iconst_0"
  emit (labelEnd ++ ":")
compileBool (EOr e1 e2) = do
  labelTrue <- newLabel
  labelEnd <- newLabel
  compileExp e1
  emit ("ifeq " ++ labelTrue)
  emit $ "iconst_1"
  emit ("goto " ++ labelEnd)
  emit (labelTrue ++ ":")
  compileExp e2
  emit (labelEnd ++ ":")