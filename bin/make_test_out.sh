#!/bin/bash
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

for t in ./test/*.cll; do
    filename=${t%.cll}
    filename=${filename##*/}
    ./bin/make_exe.zsh $filename
    echo "**********************************"
    echo "MAKING OUTPUT FOR ${filename}"
    outfile="test/${filename}.out"
    ./cll_build/$filename
    output=$(./cll_build/$filename)
    if [ $? -eq 139 ]; then
    	echo "${red}!!!ERROR: SEGFAULT!!!"

    else
        echo "${green}OUTPUT OK"
    fi
    echo "${reset}"
    echo "${output}" > $outfile
done
