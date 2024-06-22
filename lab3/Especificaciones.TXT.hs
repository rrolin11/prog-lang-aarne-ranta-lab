-- DISCLAIMER: Esta serie de comentarios no es una especificación formal 
-- ni completa de la solución, sino que solamente una guía de trabajo introductoria.

-- Inserta en la lista de instrucciones (code) una instrucción dada.
emit :: Instruction -> State Env ()

-- Asigna una dirección de memoria a una variable y la pone en el contexto del tope.
-- Se debe aumentar el contador de memoria usada para poder asignar direcciones
-- correctamente.
extendVar :: Type -> Id -> State Env ()

-- Usar extendVar para asignar direcciones de memoria a cero o más variables.
extendVars :: Type -> [Id] -> State Env ()

-- Pone el código correspondienta a la instrucción de invocación de una función en la firma de funciones.
extendFunEnv :: String -> FunType -> State Env ()

-- Obtener el la instrucción de invocación asociada a dicha función (parámetro Def), 
-- y usar la función anterior para agregarla a la firma. 
extendDef :: String -> Def -> State Env ()

-- Apila un contexto de variables vacío.
-- En caso de implementar el opcional, deben duplicar la cantidad de memoria usada hasta el momemento, 
-- es decir, duplicar la cantidad usada en el elemento de la pila previo.
newBlock :: State Env ()

-- Desapila un contexto de variables.
-- En caso de implementar el opcional, la explicación es análoga a la anterior.
exitBlock :: State Env ()

-- Aumenta el contador de etiquetas por bloque y devuelve una etiqueta nueva.
newLabel :: State Env String

-- Devuelve la instrucción de invocación asociada a dicha función dentro de la firma.
lookupFun :: Id -> State Env FunType

-- Devuelve la dirección de memoria asignada una variable.
lookupVar :: Id -> State Env Int

-- Ejecutar la mónada de estado y obtener la lista de instrucciones.
-- En caso de mantener la lista de orden inverso, recordar invertirla.
compile :: String -> Program -> [Instruction]

-- Compila una instrucción.
compileStm :: Stm -> State Env ()

-- Compila una expresión.
compileExp :: Exp -> State Env ()


  



