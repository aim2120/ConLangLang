#!/bin/bash

for t in ./test/*.cll; do
    output="$(cat $t | ./cll.native)"
    echo $t
    echo "${output}"
done
