# SELinuxIFCIL

### Requirements

Ocaml, Ocamllex, Ocamlyacc >= 4.12.0

### Overview

SELinuxIFCIL is a SELinux configuration language that extends CIL with information flow requirements.
It comes with a pair of tools for verifying that the information flow requirements are met by the configuration.

- IFCILtoNuSMV : translates a IFCIL configuration into a IFCIL-NuSMV configuration file
- IFCILverif : uses NuSMV to verify the requirements of a IFCIL-NuSMV configuration file

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
 - `IFCIL-NuSMV-config-file` : is the IFCIL-NuSMV configuration file to be verified, ontained using IFCILtoNuSMV

### Examples

The `Examples` directory contains the example from the paper and the test cases.

To verify the running example from the paper run
```
./IFCILtoNuSMV Examples/IFCILconfigurations/paper-example.cil paper-example-IFCIL-NuSMV
./IFCILverif paper-example-IFCIL-NuSMV
```

### How to Replicate Scalability Experiments of IFCILverif

The IFCIL-NuSMV configuration files for the experiments can be found in `Examples/IFCILNuSMVconfigurations`.
There is a single file for every CIL configuration and set of IFL requirements (e.g., `cilbase-pipelineNuSMVconf` is for cilbase CIL configuration and assured pipeline property). 
To replicate the results about the scalability of IFCILverif, run
```
./IFCILverif Examples/IFCILNuSMVconfigurations/cilbase-pipelineNuSMVconf
```

The script `run_experiments.sh` iterates on all the configurations and the considered properties printing the results and time of execution of IFCILverif.
```
./run_experiments.sh
```
