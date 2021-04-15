#!/bin/bash

echo > test.out
echo "***********************************" >> test.out
echo "THESE TESTS SHOULD PASS" >> test.out
echo "***********************************" >> test.out
echo "" >> test.out
for t in ./test/pass/*.cll; do
    echo "***********************************" >> test.out
    echo "TESTING FILE: ${t}" >> test.out
    echo "TESTING FILE: ${t}"
    echo "***********************************" >> test.out
    output="$(./cll.native -s $t)"
    echo "${output}" >> test.out
    echo "***********************************" >> test.out
    echo "" >> test.out
done

echo "***********************************" >> test.out
echo "THESE TESTS SHOULD FAIL" >> test.out
echo "***********************************" >> test.out
echo "" >> test.out
for t in ./test/fail/*.cll; do
    echo "***********************************" >> test.out
    echo "TESTING FILE: ${t}" >> test.out
    echo "TESTING FILE: ${t}"
    echo "***********************************" >> test.out
    output="$(./cll.native -s $t)"
    echo "${output}" >> test.out
    echo "***********************************" >> test.out
    echo "" >> test.out
done
