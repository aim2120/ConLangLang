#!/bin/bash
red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

for t in ./test/*.cll; do
    filename=${t%.cll}
    filename=${filename##*/}
    echo "**********************************"
    echo "MAKING OUTPUT FOR ${filename}"
    ./bin/make_test_exe.sh $filename
    if [ $? -eq 2 ]; then
        echo "${red}!!!NO OUTPUT FOR $filename!!!${reset}"
        continue
    fi
    outfile="test/${filename}.out"
    if [ "${filename}" = "stdin" ]; then
        cat test/test_input | ./cll_build/$filename
        output=$(cat test/test_input | ./cll_build/$filename)
    else
        ./cll_build/$filename
        output=$(./cll_build/$filename)
    fi
    if [ $? -eq 139 ]; then
    	echo "${red}!!!ERROR: SEGFAULT!!!"
        echo "${red}!!!NO OUTPUT FOR $filename!!!${reset}"

    else
        echo "${green}OUTPUT OK${reset}"
    fi
    echo "${output}" > $outfile
done
