#!/bin/bash

table="
| Properties                | Total Time          | NuSMV Running Time  |
| ------------------------- | ------------------- | ------------------- |
";
for filename in Examples/*.cil; do
    bname="$(basename -- $filename)"
    echo "----------------------------------------------"
    echo "  verifying $bname"
    echo "----------------------------------------------
"
    exec 3>&1 4>&2
    time=$( { time dune exec IFCILverif $filename 1>&3 2>&4; } 2>&1 )
    exec 3>&- 4>&-
    time="${time//[$'\r\n ']}"
    user=${time%%s*}
    user=${user#real }
    user=${user:5} 
    namelen=${#bname}
    timelen=${#user}

    exec 3>&1 4>&2
    atime=$( { time ./NuSMV NuSMVinput.tmp 1>&3 2>&4; } 2>&1 )
    exec 3>&- 4>&-
    atime="${atime//[$'\r\n ']}"
    auser=${atime%%s*}
    auser=${auser#real }
    auser=${auser:5} 
    atimelen=${#auser}

    echo $atime

    let "confpadlen = 25 - $namelen"
    let "timepadlen = 18 - $timelen"
    let "atimepadlen = 18 - $atimelen"



    let "atimepadlen = 18 - $ttimelen"
    ch=' '
    confspaces=`printf '%*s' "$confpadlen" | tr ' ' "$ch"`
    timespaces=`printf '%*s' "$timepadlen" | tr ' ' "$ch"`
    atimespaces=`printf '%*s' "$atimepadlen" | tr ' ' "$ch"`
    table="$table| $bname$confspaces | "$user"s$timespaces | "$auser"s$atimespaces |
"
done
echo "$table"
