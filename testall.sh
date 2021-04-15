#!/bin/bash

echo "***********************************"
echo "THESE TESTS SHOULD PASS"
echo "***********************************"
echo ""
for t in ./test/pass/*.cll; do
    echo "***********************************"
    echo "TESTING FILE: ${t}"
    echo "***********************************"
    output="$(./cll.native -s $t)"
    echo "${output}"
    echo "***********************************"
    echo ""
done

echo "***********************************"
echo "THESE TESTS SHOULD FAIL"
echo "***********************************"
echo ""
for t in ./test/fail/*.cll; do
    echo "***********************************"
    echo "TESTING FILE: ${t}"
    echo "***********************************"
    output="$(./cll.native -s $t)"
    echo "${output}"
    echo "***********************************"
    echo ""
done
