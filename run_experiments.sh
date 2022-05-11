#!/bin/bash

for filename in Examples/IFCILNuSMVconfigurations/*; do
    echo "verifying" $filename;
    time ./IFCILverif $filename
done
