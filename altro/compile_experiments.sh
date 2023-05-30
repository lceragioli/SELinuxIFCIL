#!/bin/bash

echo "experiment: cilbase - compiling"
time ./IFCILtoNuSMV Examples/IFCILconfigurations/cilbase.cil cilbase.out
time ./IFCILverif cilbase.out 

echo "experiment: dssp5 - compiling"
time ./IFCILtoNuSMV Examples/IFCILconfigurations/dssp5.cil dssp5.out
time ./IFCILverif dssp5.out 

echo "experiment: openWRT - compiling"
time ./IFCILtoNuSMV Examples/IFCILconfigurations/openWRT.cil openWRT.out
time ./IFCILverif openWRT.out 

 