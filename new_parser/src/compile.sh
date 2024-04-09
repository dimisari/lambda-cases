#!/bin/bash

cd ../hs_gen_results/programs
for f in *.hs
do
  ghc $f 
  rm *.hi
  rm *.o
done

shopt -s extglob
mv !(*.hs|*ll) ../compiled_progs

cd Haskell
rm *.hi
rm *.o
