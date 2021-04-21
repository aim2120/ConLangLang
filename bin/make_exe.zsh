#!/bin/zsh

file=$1
cll_file="test/${file}.cll"
ll_file="cll_build/${file}.ll"
s_file="cll_build/${file}.s"
./cll.native $cll_file > $ll_file
llc $ll_file
if [[ $# -eq 2 ]]; then
    gcc $s_file $2 -o "cll_build/${file}"
else
    gcc $s_file -o "cll_build/${file}"
fi

