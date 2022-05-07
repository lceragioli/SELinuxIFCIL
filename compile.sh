#!/bin/bash

opam switch 4.12.0
eval $(opam env)

ocamllex CILlexer.mll
ocamlyacc CILgrammar.mly
awk '/%{/{flag=1; next} /let/{flag=0} flag' CILgrammar.mly > temp
cat temp CILgrammar.mli > temp2 && mv temp2 CILgrammar.mli
ocamlc -c CILgrammar.mli
ocamlc -c CILlexer.ml
ocamlc -c CILgrammar.ml

ocamlc -c Utils.mli
ocamlc -c Utils.ml
ocamlc -c IFCILconfiguration.mli
ocamlc -c IFCILconfiguration.ml
ocamlc -c IFL.mli
ocamlc -c IFL.ml
ocamlc -c preprocessing.mli
ocamlc -c preprocessing.ml
ocamlc -c normalization.mli
ocamlc -c normalization.ml
ocamlc -c verification.mli
ocamlc -c verification.ml

ocamlc -c main.ml 
ocamlc str.cma -o main Utils.cmo CILgrammar.cmo CILlexer.cmo IFCILconfiguration.cmo IFL.cmo preprocessing.cmo normalization.cmo verification.cmo main.cmo
