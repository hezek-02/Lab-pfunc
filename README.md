# Lab-pfunc

## Descripción

Este repositorio contiene el código del laboratorio de Programación Funcional. El objetivo del laboratorio es implementar y optimizar funciones utilizando programación funcional en Haskell.

## Estructura del Proyecto

- `AST.hs`: Define la estructura de los árboles de sintaxis abstracta (AST) utilizados en el proyecto.
- `PrettyPrint.hs`: Este módulo incluye funciones que permiten formatear y mostrar los Árboles de Sintaxis Abstracta (AST) de una manera legible y estructurada.
- `Linter.hs`: Contiene el código del linter que sugiere optimizaciones para el código Haskell.
- `LintTypes.hs`: Define los tipos utilizados por el linter.
- `Lintings.hs`: Implementa las diferentes optimizaciones que el linter puede sugerir.
- `ejemplos/`: Contiene ejemplos de código Haskell y sus respectivas sugerencias de optimización.

## Uso

Para ejecutar el linter y obtener sugerencias de optimización, puedes usar los siguientes comandos:

```sh
    ghci ./Linter.hs
```

```sh
    ./Linter -<opt> -<linting> 'archivo-ejemplo'
```

## Lintings `<linting>`

- lintComputeConstant
- lintRedBool
- lintRedIfCond
- lintRedIfAnd
- lintRedIfOr
- lintNull
- lintAppend
- lintComp
- lintEta
- lintMap

## Options `<opt>`

- `-s`: Imprime sugerencias en la salida estándar.
- `-v`: Imprime el AST de las sugerencias en la salida estándar.
- `-c`: Aplica las sugerencias e imprime el programa resultante.

## Observaciones

- Si `<linting>` es vacío, se ejecutan todos los lintings
- Ejemplo de ejecución: ```./Linter  -s -lintComputeConstant  ejemplos/ejemplo1-lint.mhs```
