#!/bin/bash

red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

file=$1
filename="${file##*/}"
filename="${filename%%.*}"
build_dir="cll_build"

ll_file="${build_dir}/${filename}.ll"
s_file="${build_dir}/${filename}.s"
lib_files=("${build_dir}/hash_table.o" "${build_dir}/find_prime.o" "${build_dir}/linked_list.o" "${build_dir}/regex.o" "${build_dir}/malloc_manager.o" "${build_dir}/stdin.o")

./cll.native $file > $ll_file
if [ $? -eq 2 ]; then
    echo "${red}!!!ERROR CLL.NATIVE FAILED!!${reset}"
    cat $ll_file
    exit 2
fi
llc -relocation-model=pic $ll_file > $s_file
gcc -o "${filename}" $s_file ${lib_files[@]}

