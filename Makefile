all: IFCILtoNuSMV IFCILverif

cil:
	ocamlc -c CIL/CILsyntax.mli
	ocamlc -c -I CIL CIL/CILsyntax.ml
	ocamllex parser/CILlexer.mll
	ocamlyacc parser/CILparser.mly
	ocamlc -c -I CIL parser/CILparser.mli
	ocamlc -c -I CIL -I parser parser/CILlexer.ml
	ocamlc -c -I CIL -I parser parser/CILparser.ml
	ocamlc -c -I CIL CIL/CILenv.mli
	ocamlc -c -I CIL CIL/CILenv.ml
	ocamlc -c -I CIL -I parser tests/testingCIL.ml
	ocamlc -I CIL -I parser -I tests str.cma -o testCIL CILenv.cmo CILparser.cmo CILlexer.cmo CILsyntax.cmo testingCIL.cmo

ifcil:
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

IFCILtoNuSMV: ifcil
	ocamlc -c IFCILtoNuSMV.ml 
	ocamlc str.cma -o IFCILtoNuSMV Utils.cmo CILgrammar.cmo CILlexer.cmo IFCILconfiguration.cmo IFL.cmo preprocessing.cmo normalization.cmo IFCILtoNuSMV.cmo

IFCILverif: ifcil
	ocamlc -c IFCILverif.ml 
	ocamlc str.cma -o IFCILverif Utils.cmo CILgrammar.cmo CILlexer.cmo IFCILconfiguration.cmo IFL.cmo preprocessing.cmo normalization.cmo IFCILverif.cmo

clean:
	rm *.cmo *.cmi CILlexer.ml CILgrammar.ml CILgrammar.mli