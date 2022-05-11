#!/bin/bash

# You may need to run the following before compiling
# opam switch 4.12.0
# eval $(opam env)

ocamlc -c CILsyntax.ml

ocamllex CILlexer.mll
ocamlyacc CILgrammar.mly
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

ocamlc -c IFCILtoNuSMV.ml 
ocamlc str.cma -o IFCILtoNuSMV Utils.cmo CILgrammar.cmo CILlexer.cmo IFCILconfiguration.cmo IFL.cmo preprocessing.cmo normalization.cmo IFCILtoNuSMV.cmo

ocamlc -c IFCILverif.ml 
ocamlc str.cma -o IFCILverif Utils.cmo CILgrammar.cmo CILlexer.cmo IFCILconfiguration.cmo IFL.cmo preprocessing.cmo normalization.cmo IFCILverif.cmo
