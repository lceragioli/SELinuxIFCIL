#!/bin/bash

table="
| Properties                | Verification Time         |
| ------------------------- | ------------------------- |
";
for filename in Examples/IFCILNuSMVconfigurations/*; do
    bname="$(basename -- $filename)"
    echo "----------------------------------------------"
    echo "  verifying $bname"
    echo "----------------------------------------------
"
    exec 3>&1 4>&2
    time=$( { time ./IFCILverif $filename 1>&3 2>&4; } 2>&1 )
    exec 3>&- 4>&-
#get everything to the left of the first "s*"
    time="${time//[$'\r\n ']}"
    user=${time%%s*}
#get everything to the right of first "*user "
    user=${user#real }
#    user=${user#[[:digit:]]}
    user=${user:5} 
    namelen=${#bname}
    timelen=${#user}
    let "confpadlen = 25 - $namelen"
    let "timepadlen = 24 - $timelen"
    ch=' '
    confspaces=`printf '%*s' "$confpadlen" | tr ' ' "$ch"`
    timespaces=`printf '%*s' "$timepadlen" | tr ' ' "$ch"`
    table="$table| $bname$confspaces | "$user"s$timespaces |
"
done
echo "$table"
