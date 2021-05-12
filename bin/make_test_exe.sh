#!/bin/bash

file=$1
src_dir="test"
build_dir="cll_build"

cll_file="${src_dir}/${file}.cll"
ll_file="${build_dir}/${file}.ll"
s_file="${build_dir}/${file}.s"
lib_files=("${build_dir}/hash_table.o" "${build_dir}/find_prime.o" "${build_dir}/linked_list.o" "${build_dir}/regex.o" "${build_dir}/malloc_manager.o" "${build_dir}/stdin.o")

./cll.native $cll_file 2>&1 > $ll_file
if [ $? -eq 2 ]; then
    echo "!!!ERROR CLL.NATIVE FAILED!!"
    exit 2
fi
llc -relocation-model=pic $ll_file > $s_file
if [[ $# -eq 2 ]]; then
    gcc -o "${build_dir}/${file}" $s_file ${lib_files[@]} $2
else
    gcc -o "${build_dir}/${file}" $s_file ${lib_files[@]}
fi

