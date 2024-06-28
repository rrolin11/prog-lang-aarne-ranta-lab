# Laboratorio: Implementando lenguajes de programación

En este repositorio se implementan las tareas planteadas en el libro "Implementing Programming Languages" de Aarne Ranta ([website del libro](https://www.grammaticalframework.org/ipl-book)).

## Tareas:
1. [Parser para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment1/assignment1.html)
2. [Verificador de típos para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment2/assignment2.html)
3. [Generador de código para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment4/assignment4.html)

## Dependencias

Será necesario contar con BNFC Converter, Make, Happy, Alex para poder compilar las gramáticas y ejecutar pruebas. Adicionalmente, para el compilador se requiere JDK.

## Tarea 1

* __bnfc -m "CPP.cf"__: Compila el parser y lexer generando sus archivos junto al makefile.
* __make__: Genera el archivo ejecutable TestCPP.exe
* __TestCPP.exe < test/[archivo test].cc__: Ejecuta el programa de test con el input que contiene el archivo .cc.

## Tarea 2

* El archivo de inicio es TypeChecker.hs, en la función typecheck comienza el chequeo de tipos.
* __progs-test-lab2.hs__: Situado dentro de la carpeta __test__. Se compila con el comando ghc y con éste se realizan todos los casos de prueba. Ejecutar con el comando "progs-test-lab2-hs ..".
* __make__: Genera el archivo ejecutable lab2.exe
* __lab2.exe < test/[carpeta]/[archivo test].cc__: Ejecuta el programa con el input que contiene el archivo .cc.

## Tarea 3

* El proyecto requiere tener una carpeta good (contiene casos de prueba correctos, ya que se asume que pasaron por el Typechecker) y una carpeta bad (vacía), para poder realizar pruebas automatizadas.
* El archivo Compiler.hs implementa el compilador y todas las instrucciones equivalentes para la JVM. La función de entrada es compile.
* Es necesario instalar JDK para utilizar Jasmine.
* __ccpp.hs__: Consume los programas .cc y los envía al Compiler.hs.
* __make__: Genera el archivo ejecutable lab2.exe
* __progs-test-lab3.hs__: Se compila con el comando GHC. Genera un ejecutable el cual se corre con el comando "./progs-test-lab3.exe ." y ejecuta todas las pruebas atomáticas.
