#!/bin/bash

ghc Linter.hs 

./Linter -c casos/caso01.mhs > casos/misalida/caso01-lint.mhs
./Linter -c casos/caso02.mhs > casos/misalida/caso02-lint.mhs
./Linter -c casos/caso03.mhs > casos/misalida/caso03-lint.mhs
./Linter -c casos/caso04.mhs > casos/misalida/caso04-lint.mhs
./Linter -c casos/caso05.mhs > casos/misalida/caso05-lint.mhs
./Linter -c casos/caso06.mhs > casos/misalida/caso06-lint.mhs
./Linter -c casos/caso07.mhs > casos/misalida/caso07-lint.mhs
./Linter -c casos/caso08.mhs > casos/misalida/caso08-lint.mhs
./Linter -c casos/caso09.mhs > casos/misalida/caso09-lint.mhs
./Linter -c casos/caso10.mhs > casos/misalida/caso10-lint.mhs
./Linter -c casos/caso11.mhs > casos/misalida/caso11-lint.mhs
./Linter -c casos/caso12.mhs > casos/misalida/caso12-lint.mhs
./Linter -c casos/caso13.mhs > casos/misalida/caso13-lint.mhs
./Linter -c casos/caso14.mhs > casos/misalida/caso14-lint.mhs
./Linter -c casos/caso15.mhs > casos/misalida/caso15-lint.mhs
./Linter -c casos/caso16.mhs > casos/misalida/caso16-lint.mhs
./Linter -c casos/caso17.mhs > casos/misalida/caso17-lint.mhs
./Linter -c casos/caso18.mhs > casos/misalida/caso18-lint.mhs
./Linter -c casos/caso19.mhs > casos/misalida/caso19-lint.mhs
./Linter -c casos/caso20.mhs > casos/misalida/caso20-lint.mhs
./Linter -c casos/caso21.mhs > casos/misalida/caso21-lint.mhs
./Linter -c casos/caso22.mhs > casos/misalida/caso22-lint.mhs
./Linter -c casos/caso23.mhs > casos/misalida/caso23-lint.mhs
./Linter -c casos/caso24.mhs > casos/misalida/caso24-lint.mhs

./Linter -s casos/caso01.mhs > casos/misalida/caso01-sug
./Linter -s casos/caso02.mhs > casos/misalida/caso02-sug
./Linter -s casos/caso03.mhs > casos/misalida/caso03-sug
./Linter -s casos/caso04.mhs > casos/misalida/caso04-sug
./Linter -s casos/caso05.mhs > casos/misalida/caso05-sug
./Linter -s casos/caso06.mhs > casos/misalida/caso06-sug
./Linter -s casos/caso07.mhs > casos/misalida/caso07-sug
./Linter -s casos/caso08.mhs > casos/misalida/caso08-sug
./Linter -s casos/caso09.mhs > casos/misalida/caso09-sug
./Linter -s casos/caso10.mhs > casos/misalida/caso10-sug
./Linter -s casos/caso11.mhs > casos/misalida/caso11-sug
./Linter -s casos/caso12.mhs > casos/misalida/caso12-sug
./Linter -s casos/caso13.mhs > casos/misalida/caso13-sug
./Linter -s casos/caso14.mhs > casos/misalida/caso14-sug
./Linter -s casos/caso15.mhs > casos/misalida/caso15-sug
./Linter -s casos/caso16.mhs > casos/misalida/caso16-sug
./Linter -s casos/caso17.mhs > casos/misalida/caso17-sug
./Linter -s casos/caso18.mhs > casos/misalida/caso18-sug
./Linter -s casos/caso19.mhs > casos/misalida/caso19-sug
./Linter -s casos/caso20.mhs > casos/misalida/caso20-sug
./Linter -s casos/caso21.mhs > casos/misalida/caso21-sug
./Linter -s casos/caso22.mhs > casos/misalida/caso22-sug
./Linter -s casos/caso23.mhs > casos/misalida/caso23-sug
./Linter -s casos/caso24.mhs > casos/misalida/caso24-sug

# Para los archivos de lint
for i in {01..24}
do
  echo "Procesando archivo caso${i}-lint.mhs"
  diff -w casos/salidas/caso${i}-lint.mhs casos/misalida/caso${i}-lint.mhs > casos/dif/diferencias_caso${i}-lint.txt
done

# Para los archivos de sugerencias
for i in {01..24}
do
  echo "Procesando archivo caso${i}-sug"
  diff -w casos/salidas/caso${i}-sug casos/misalida/caso${i}-sug > casos/dif/diferencias_caso${i}-sug.txt
done
