# SELinuxIFCIL

### Requirements

Ocaml, Ocamllex, Ocamlyacc >= 4.12.0

### Overview

SELinux IFCIL is a SELinux configuration language that extends CIL with information flow requirements.
IFCIL configurations are legal CIL configurations and can be compiled using the standard SELinux CIL compiler.
Moreover, IFCIL comes with a pair of tools for verifying that the information flow requirements are met by the configuration.

- IFCILtoNuSMV : translates a IFCIL configuration into a IFCIL-NuSMV configuration file
- IFCILverif : uses the model checker NuSMV to verify the requirements of a IFCIL-NuSMV configuration file

### Compilation

To compile run
```
make
```
It will produce the executables `IFCILtoNuSMV` and `IFCILverif`

Depending on you system, you may need to run the following before compiling to use the correct version of ocaml

```
opam switch 4.12.0
eval $(opam env)
```

### How to Generate the IFCIL-NuSMV configuration file

To generate the configuration file for NuSMV with IFCIL labels run
```
./IFCILtoNuSMV IFCIL-input-file IFCIL-NuSMV-output-file  
```
Where
 - `IFCILtoNuSMV` : is the name of the executable
 - `IFCIL-input-file` : is the IFCIL input file, i.e., a SELinux CIL configuration with IFL annotations
 - `IFCIL-NuSMV-output-file` : is the destination for the IFCIL-NuSMV configuration file to be verified using the tool IFCILverif

### How to Verify a IFCIL-NuSMV configuration file

To verify the IFCIL-NuSMV configuration file run
```
./IFCILverif IFCIL-NuSMV-config-file  
```
Where
 - `IFCILverif` : is the name of the executable
 - `IFCIL-NuSMV-config-file` : is the IFCIL-NuSMV configuration file to be verified, obtained using IFCILtoNuSMV

### Examples

The `Examples` directory contains the running example from the paper and the real-world tests.

To verify the running example from the paper run
```
./IFCILtoNuSMV Examples/IFCILconfigurations/paper-example.cil paper-example-IFCIL-NuSMV
./IFCILverif paper-example-IFCIL-NuSMV
```

### How to Replicate the Experiments about Scalability of IFCILverif

The IFCIL-NuSMV configuration files for the experiments can be found in `Examples/IFCILNuSMVconfigurations`.
There is a single file for every CIL configuration and set of IFL requirements (e.g., `cilbase-pipelineNuSMVconf` is for cilbase CIL configuration and assured pipeline property). 
To replicate the results about the scalability of IFCILverif, run
```
./IFCILverif Examples/IFCILNuSMVconfigurations/cilbase-pipelineNuSMVconf
```

The script `run_experiments.sh` iterates on all the configurations and the considered properties printing the output of IFCILverif and the time of execution measured through the standard Unix command `time`.
Table 1 of the paper lists the configuration, properties and the execution time obtained through the `time` command.
```
./run_experiments.sh
```

The expected execution time is as follows

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
 Examples/                 <-- Running example from the paper and real-world policies

 README.md                 <-- This file

 CILlexer.mll              <-- Ocamllex configuration file
 CILgrammar.mly            <-- Ocamlyacc configuration file

 CILsyntax.ml              <-- Source for CIL language syntax
 IFL.mli                   <-- Interface for IFL language syntax and refinement 
 IFL.ml                    <-- Source for IFL language syntax and refinement 
 IFCILconfiguration.mli    <-- Interface for IFLCIL configurations 
 IFCILconfiguration.ml     <-- Source for IFLCIL configurations 
 normalization.mli         <-- Interface for the normalization pipeline of IFCIL  
 normalization.ml          <-- Source for the normalization pipeline of IFCIL
 preprocessing.mli         <-- Interface for preprocessing on IFCIL configurations   
 preprocessing.ml          <-- Source for preprocessing on IFCIL configurations
 Utils.mli                 <-- Interface for utilities data structures and functions 
 Utils.ml                  <-- Source for utilities data structures and functions  

 IFCILtoNuSMV.ml           <-- Source for IFCILtoNuSMV tool
 IFCILverif.ml             <-- Source for IFCILverif tool

 NuSMV                     <-- NuSMV model checker executable 

 Makefile                  
 run_experiments.sh        <-- Script for the scalability experiments
```
