#!/bin/bash

echo "***********************************"
echo "THESE TESTS SHOULD PASS"
echo "***********************************"
for t in ./test/pass/*.cll; do
    output="$(./cll.native -s $t)"
    echo "***********************************"
    echo "TESTING FILE: ${t}"
    echo "***********************************"
    echo "${output}"
    echo "***********************************"
    echo ""
done

echo "***********************************"
echo "THESE TESTS SHOULD FAIL"
echo "***********************************"
for t in ./test/fail/*.cll; do
    output="$(./cll.native -s $t)"
    echo "***********************************"
    echo "TESTING FILE: ${t}"
    echo "***********************************"
    echo "${output}"
    echo "***********************************"
    echo ""
done
