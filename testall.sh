#!/bin/bash

for t in ./test/*.cll; do
    output="$(./cll.native -s $t)"
    echo "***********************************"
    echo "TESTING FILE: ${t}"
    echo "***********************************"
    echo "${output}"
    echo "***********************************"
    echo ""
done
