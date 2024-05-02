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

| Properties                | Total Time          | NuSMV Running Time  |
| ------------------------- | ------------------- | ------------------- |
| cilbase-augment.cil       | 0m3,349s            | 0m0,191s            |
| cilbase.cil               | 0m3,962s            | 0m0,515s            |
| cilbase-pipeline.cil      | 0m3,373s            | 0m0,194s            |
| cilbase-TCB.cil           | 0m3,256s            | 0m0,099s            |
| cilbase-wrapping.cil      | 0m3,363s            | 0m0,227s            |
| dssp5.cil                 | 0m7,735s            | 0m1,969s            |
| dssp5-pipeline.cil        | 0m6,321s            | 0m0,806s            |
| dssp5-TCB.cil             | 0m6,321s            | 0m0,546s            |
| dssp5-wrapping.cil        | 0m7,037s            | 0m1,299s            |
| openWRT-augment.cil       | 0m50,687s           | 0m9,961s            |
| openWRT.cil               | 1m13,219s           | 0m32,465s           |
| openWRT-pipeline.cil      | 0m50,719s           | 0m10,227s           |
| openWRT-TCB.cil           | 0m46,107s           | 0m5,011s            |
| openWRT-wrapping.cil      | 0m56,318s           | 0m15,147s           |
| paper-example.cil         | 0m0,048s            | 0m0,013s            |


### Project structure

Here is a description of content of the repository

```
Examples/                      <-- Running example from the paper and real-world policies

bin/                           <-- directory for the application IFCILverif
   IFCILverif.ml               <-- our application source code
   ReadNuSMV.ml                <-- module for interacting with the NuSMV model checker
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
