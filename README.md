# Lenguajes y Compiladores - Primer semestre de 2024

En este repositorio se agregan los laboratorios de construcción de un compilador.

El libro de referencia usado para estos laboratorios es "Implementing Programming Languages" de Aarne Ranta.

Website del libro: <https://www.grammaticalframework.org/ipl-book>

## Dependencias

Será necesario contar con BNFC Converter, Make, Happy y Alex para poder compilar las gramáticas y ejecutar pruebas.

## Comandos

* __bnfc -m "CPP.cf"__: Compila el parser y lexer generando sus archivos junto al makefile.
* __make__: Genera el archivo ejecutable TestCPP.exe
* __TestCPP.exe < test/[archivo test].cc__: Ejecuta el programa de test con el input que contiene el archivo .cc.