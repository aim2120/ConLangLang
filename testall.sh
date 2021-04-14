#!/bin/bash

for t in ./test/*.cll; do
    output="$(cat $t | ./cll.native)"
    echo "***********************************"
    echo "TESTING FILE: ${t}"
    echo "***********************************"
    echo "${output}"
    echo "***********************************"
    echo ""
done
