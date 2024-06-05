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

| Properties                                                                       | Total Time            | NuSMV Running Time    |
| -------------------------------------------------------------------------------- | --------------------- | --------------------- |
| __aosp_marlin-n2g47e-factory-ebc79a0b_sepolicy.cil                               | 0m15,823s             | 0m12,335s             |
| __aosp_marlin-n2g47e-factory-ebc79a0b_sepolicy-cw.cil                            | 0m9,501s              | 0m7,202s              |
| __aosp_marlin-n2g47e-factory-ebc79a0b_sepolicy-enc.cil                           | 0m5,093s              | 0m2,520s              |
| __aosp_marlin-n2g47e-factory-ebc79a0b_sepolicy-tcb.cil                           | 0m9,537s              | 0m6,918s              |
| __aosp_sailfish-njh47f-factory-6fd9b2c4_sepolicy.cil                             | 0m15,269s             | 0m12,272s             |
| __aosp_sailfish-njh47f-factory-6fd9b2c4_sepolicy-cw.cil                          | 0m9,488s              | 0m7,174s              |
| __aosp_sailfish-njh47f-factory-6fd9b2c4_sepolicy-enc.cil                         | 0m4,544s              | 0m2,515s              |
| __aosp_sailfish-njh47f-factory-6fd9b2c4_sepolicy-tcb.cil                         | 0m8,105s              | 0m6,075s              |
| __aosp_sailfish-opm1_171019_011-factory-56d15350_precompiled_sepolicy.cil        | 0m36,289s             | 0m29,756s             |
| __aosp_sailfish-opm1_171019_011-factory-56d15350_precompiled_sepolicy-cw.cil     | 0m22,644s             | 0m16,495s             |
| __aosp_sailfish-opm1_171019_011-factory-56d15350_precompiled_sepolicy-enc.cil    | 0m12,985s             | 0m7,093s              |
| __aosp_sailfish-opm1_171019_011-factory-56d15350_precompiled_sepolicy-tcb.cil    | 0m21,969s             | 0m15,699s             |
| __aosp_sailfish-ppr2_181005_003_a1-factory-dec6298c_precompiled_sepolicy.cil     | 0m54,719s             | 0m45,100s             |
| __aosp_sailfish-ppr2_181005_003_a1-factory-dec6298c_precompiled_sepolicy-cw.cil  | 0m34,175s             | 0m24,131s             |
| __aosp_sailfish-ppr2_181005_003_a1-factory-dec6298c_precompiled_sepolicy-enc.cil | 0m19,717s             | 0m9,908s              |
| __aosp_sailfish-ppr2_181005_003_a1-factory-dec6298c_precompiled_sepolicy-tcb.cil | 0m34,176s             | 0m24,237s             |
| __aosp_sailfish-pq2a_190205_003-factory-164a7269_precompiled_sepolicy.cil        | 0m55,148s             | 0m44,883s             |
| __aosp_sailfish-pq2a_190205_003-factory-164a7269_precompiled_sepolicy-cw.cil     | 0m33,822s             | 0m24,532s             |
| __aosp_sailfish-pq2a_190205_003-factory-164a7269_precompiled_sepolicy-enc.cil    | 0m19,617s             | 0m9,774s              |
| __aosp_sailfish-pq2a_190205_003-factory-164a7269_precompiled_sepolicy-tcb.cil    | 0m34,121s             | 0m24,401s             |
| cilbase-augment.cil                                                              | 0m3,417s              | 0m0,191s              |
| cilbase.cil                                                                      | 0m4,009s              | 0m0,514s              |
| cilbase-pipeline.cil                                                             | 0m3,396s              | 0m0,190s              |
| cilbase-TCB.cil                                                                  | 0m3,325s              | 0m0,100s              |
| cilbase-wrapping.cil                                                             | 0m3,561s              | 0m0,225s              |
| dssp5.cil                                                                        | 0m8,135s              | 0m2,050s              |
| dssp5-pipeline.cil                                                               | 0m6,560s              | 0m0,818s              |
| dssp5-TCB.cil                                                                    | 0m6,523s              | 0m0,556s              |
| dssp5-wrapping.cil                                                               | 0m7,514s              | 0m1,298s              |
| openWRT-augment.cil                                                              | 0m53,323s             | 0m10,277s             |
| openWRT.cil                                                                      | 1m15,182s             | 0m32,721s             |
| openWRT-pipeline.cil                                                             | 0m52,766s             | 0m10,084s             |
| openWRT-TCB.cil                                                                  | 0m49,236s             | 0m5,109s              |
| openWRT-wrapping.cil                                                             | 0m57,904s             | 0m15,232s             |
| paper-example.cil                                                                | 0m0,054s              | 0m0,013s              |
| __samsung_G935FXXU1DPLT_G935FOXA1DPLT_BTU_sepolicy.cil                           | 1m39,030s             | 1m27,606s             |
| __samsung_G935FXXU1DPLT_G935FOXA1DPLT_BTU_sepolicy-cw.cil                        | 0m57,247s             | 0m45,493s             |
| __samsung_G935FXXU1DPLT_G935FOXA1DPLT_BTU_sepolicy-enc.cil                       | 0m24,561s             | 0m12,868s             |
| __samsung_G935FXXU1DPLT_G935FOXA1DPLT_BTU_sepolicy-tcb.cil                       | 0m57,855s             | 0m46,403s             |
| __samsung_P555MUBU1CQI6_P555MUUB1CQJ1_COO_sepolicy.cil                           | 2m55,894s             | 2m45,634s             |
| __samsung_P555MUBU1CQI6_P555MUUB1CQJ1_COO_sepolicy-cw.cil                        | 2m56,061s             | 2m44,882s             |
| __samsung_P555MUBU1CQI6_P555MUUB1CQJ1_COO_sepolicy-enc.cil                       | 2m55,850s             | 2m45,451s             |
| __samsung_P555MUBU1CQI6_P555MUUB1CQJ1_COO_sepolicy-tcb.cil                       | 2m55,741s             | 2m45,376s             |



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
