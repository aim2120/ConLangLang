#!/bin/zsh

file=$1
src_dir="test"
build_dir="cll_build"

cll_file="${src_dir}/${file}.cll"
ll_file="${build_dir}/${file}.ll"
s_file="${build_dir}/${file}.s"

./cll.native $cll_file > $ll_file
llc -relocation-model=pic $ll_file > $s_file
if [[ $# -eq 2 ]]; then
    cc -o $s_file lib/c_libraries.c $2 "${build_dir}/${file}"
else
    cc -o "${build_dir}/${file}" $s_file
fi

