#!/bin/bash

red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

file=$1
src_dir="test"
build_dir="cll_build"

cll_file="${src_dir}/${file}.cll"
ll_file="${build_dir}/${file}.ll"
s_file="${build_dir}/${file}.s"
lib_files=("${build_dir}/hash_table.o" "${build_dir}/find_prime.o" "${build_dir}/linked_list.o" "${build_dir}/regex.o" "${build_dir}/malloc_manager.o")

./cll.native $cll_file > $ll_file
if [ $? -eq 2 ]; then
    echo "${red}!!!ERROR CLL.NATIVE FAILED!!${reset}"
    cat $ll_file
    exit 2
fi
llc -relocation-model=pic $ll_file > $s_file
if [[ $# -eq 2 ]]; then
    gcc -o "${build_dir}/${file}" $s_file ${lib_files[@]} $2
else
    gcc -o "${build_dir}/${file}" $s_file ${lib_files[@]}
fi

