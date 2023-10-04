# SELinuxIFCIL

### Requirements

Ocaml, Ocamllex, Ocamlyacc >= 4.12.0

Dune >= 3.8.1

### Overview

SELinux IFCIL is a SELinux configuration language that extends CIL with information flow requirements.
IFCIL configurations are legal CIL configurations and can be compiled using the standard SELinux CIL compiler.
Moreover, IFCIL comes with IFCILverif, a tool for verifying that the information flow requirements are met by the configuration.

### Compilation

To compile run
```
dune build
```
Depending on you system, you may need to run the following before compiling to use the correct version of ocaml

```
opam switch 4.12.0
eval $(opam env)
```

### How to Verify a IFCIL-NuSMV configuration file

To verify the IFCIL configuration file run
```
./IFCILverif IFCIL-input-file  
```
Where
 - `IFCILverif` : is the name of the executable
 - `IFCIL-input-file` : is the IFCIL input file, i.e., a SELinux CIL configuration with IFL annotations

### Examples

The `Examples` directory contains the running example from the paper and the real-world tests.

To verify the running example from the paper run
```
./IFCILtoNuSMV Examples/paper-example.cil
```

### How to Replicate the Experiments about Scalability of IFCILverif

The IFCIL configuration files for the experiments can be found in `Examples`.
There is a single file for every CIL configuration and set of IFL requirements (e.g., `cilbase-pipeline` is for cilbase CIL configuration and assured pipeline property). 
To replicate the results about the scalability of IFCILverif, run
```
./IFCILverif Examples/cilbase-pipeline.cil
```

The script `run_experiments.sh` iterates on all the configurations and the considered properties printing the output of IFCILverif and the time of execution measured through the standard Unix command `time`.
Table 1 of the paper lists the configuration, properties and the execution time obtained through the `time` command.
```
./run_experiments.sh
```

The expected results are in `expected_results.txt` and the execution time should be approximately as follows

| Properties                | Verification Time         |
| ------------------------- | ------------------------- |
| cilbase-augmentNuSMVconf  | 0m 0,103s                 |
| cilbaseNuSMVconf          | 0m 0,287s                 |
| cilbase-pipelineNuSMVconf | 0m 0,113s                 |
| cilbase-TCBNuSMVconf      | 0m 0,053s                 |
| cilbase-wrappingNuSMVconf | 0m 0,128s                 |
| dssp5NuSMVconf            | 0m 0,427s                 |
| dssp5-pipelineNuSMVconf   | 0m 0,180s                 |
| dssp5-TCBNuSMVconf        | 0m 0,132s                 |
| dssp5-wrappingNuSMVconf   | 0m 0,279s                 |
| openWRT-augmentNuSMVconf  | 0m 7,420s                 |
| openWRTNuSMVconf          | 0m 27,469s                |
| openWRT-pipelineNuSMVconf | 0m 8,079s                 |
| openWRT-TCBNuSMVconf      | 0m 3,346s                 |
| openWRT-wrappingNuSMVconf | 0m 9,850s                 |
| paper-exampleNuSMVconf    | 0m 0,021s                 |


### Project structure

Here is a description of content of the repository

```
Examples/                      <-- Running example from the paper and real-world policies

bin/                           <-- directory for applications using our semantics which comes as a library
   computeSem                  <-- our example application that just prints the allows of a configuration
   dune                        <-- dune configuration file

lib/                           <-- our SELinux IFCIL library
   IFCILlexer.mll              <-- specification file for ocamllex
   IFCILparser.mly             <-- specification file for ocamlyacc
   IFCILsyntax.ml - .mli       <-- module for abstract syntax of CIL
   IFCILsyntaxE.ml - .mli      <-- module for an efficient representation of CIL configurations
   IFCILsemanticsE.ml - .mli   <-- module for IFCIL semantics 
   CILclass.ml - .mli          <-- module with class and operations related functions
   CILenv.ml - .mli            <-- module for environment rho and frame fr
   CILenvE.ml - .mli           <-- module for an efficient representation of environment rho and frame fr
   IFL.ml - .mli               <-- module for IFL information flow language
   Utils.ml                    <-- module with utilities data structures and functions 
   dune                        <-- dune configuration file

NuSMV                          <-- NuSMV model checker executable 
dune                           <-- dune configuration file
dune-project                   <-- dune configuration file

run_experiments.sh             <-- Script for the scalability experiments
expected_results.txt           <-- Expected results of run_experiments.sh
README.md                      <-- This file

```
