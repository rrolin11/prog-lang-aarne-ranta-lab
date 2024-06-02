# Laboratorio: Implementando lenguajes de programación

En este repositorio se implementan las tareas planteadas en el libro "Implementing Programming Languages" de Aarne Ranta ([website del libro](https://www.grammaticalframework.org/ipl-book)).

## Tareas:
1. [Parser para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment1/assignment1.html)
2. [Verificador de típos para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment2/assignment2.html)
3. [Intérprete para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment3/assignment3.html)
4. [Generador de código para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment4/assignment4.html)
5. [Intérprete para Fun](https://www.grammaticalframework.org/ipl-book/assignments/assignment5/assignment5.html)
6. [Un lenguaje de propósito especial](https://www.grammaticalframework.org/ipl-book/assignments/assignment6/assignment6.html)

## Dependencias

Será necesario contar con BNFC Converter, Make, Happy y Alex para poder compilar las gramáticas y ejecutar pruebas.

## Tarea 1

* __bnfc -m "CPP.cf"__: Compila el parser y lexer generando sus archivos junto al makefile.
* __make__: Genera el archivo ejecutable TestCPP.exe
* __TestCPP.exe < test/[archivo test].cc__: Ejecuta el programa de test con el input que contiene el archivo .cc.

## Tarea 2

* El archivo de inicio es TypeChecker.hs, en la función typecheck comienza el chequeo de tipos.
* __progs-test-lab2.hs__: Situado dentro de la carpeta __test__. Se compila con el comando ghc y con éste se realizan todos los casos de prueba. Ejecutar con el comando "progs-test-lab2-hs ..".
* __make__: Genera el archivo ejecutable lab2.exe
* __lab2.exe < test/[carpeta]/[archivo test].cc__: Ejecuta el programa con el input que contiene el archivo .cc.
