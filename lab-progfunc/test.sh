#!/bin/bash

rm -f Linter *.o *.hi # Eliminar archivos anteriores
rm -f casos/misalida/* casos/dif/* # Eliminar archivos de salida anteriores

# Compilar Linter.hs
ghc Linter.hs 

# Crear directorios si no existen
mkdir -p casos/misalida casos/dif

# Ejecutar Linter para cada caso y generar archivos de salida
for i in {01..24}
do
  echo "------------------------------------------------------------------"
  
  # Ejecutar con opción -c y guardar la salida
  ./Linter -c casos/caso${i}.mhs > casos/misalida/caso${i}-lint.mhs
  
  # Ejecutar con opción -s y guardar la salida
  ./Linter -s casos/caso${i}.mhs > casos/misalida/caso${i}-sug
  
  # Comparar archivos de lint
  diff -w casos/salidas/caso${i}-lint.mhs casos/misalida/caso${i}-lint.mhs > casos/dif/diferencias_caso${i}-lint.txt
  if [ -s casos/dif/diferencias_caso${i}-lint.txt ]; then
    echo "Caso ${i} -lint: MAL. Ver diferencias en casos/dif/diferencias_caso${i}-lint.txt"
  else
    echo "Caso ${i} -lint: OK."
    rm casos/dif/diferencias_caso${i}-lint.txt # Eliminar archivo si no hay diferencias
  fi
  
  # Comparar archivos de sugerencias
  diff -w casos/salidas/caso${i}-sug casos/misalida/caso${i}-sug > casos/dif/diferencias_caso${i}-sug.txt
  if [ -s casos/dif/diferencias_caso${i}-sug.txt ]; then
    echo "Caso ${i} -sug: MAL. Ver diferencias en casos/dif/diferencias_caso${i}-sug.txt"
  else
    echo "Caso ${i} -sug: OK."
    rm casos/dif/diferencias_caso${i}-sug.txt # Eliminar archivo si no hay diferencias
  fi
done