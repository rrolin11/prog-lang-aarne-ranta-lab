# Lenguajes y Compiladores - Primer semestre de 2024

En este repositorio se implementan las tareas planteadas en el libro [Implementing Programming Languages - Aarne Ranta](https://www.grammaticalframework.org/ipl-book):
* [1. Parser para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment1/assignment1.html)
* [2. Verificador de típos para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment2/assignment2.html)
* [3. Intérprete para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment3/assignment3.html)
* [4. Generador de código para C++](https://www.grammaticalframework.org/ipl-book/assignments/assignment4/assignment4.html)
* [5. Intérprete para Fun](https://www.grammaticalframework.org/ipl-book/assignments/assignment5/assignment5.html)
* [6. Un lenguaje de propósito especial](https://www.grammaticalframework.org/ipl-book/assignments/assignment6/assignment6.html)

## Dependencias

Será necesario contar con BNFC Converter, Make, Happy y Alex para poder compilar las gramáticas y ejecutar pruebas.

## Comandos

* __bnfc -m "CPP.cf"__: Compila el parser y lexer generando sus archivos junto al makefile.
* __make__: Genera el archivo ejecutable TestCPP.exe
* __TestCPP.exe < test/[archivo test].cc__: Ejecuta el programa de test con el input que contiene el archivo .cc.
