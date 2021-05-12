#!/bin/zsh

red=`tput setaf 1`
green=`tput setaf 2`
reset=`tput sgr0`

src_dir="test"
build_dir="cll_build"

touch test.out
output="***********************************\n"
output+="THESE TESTS SHOULD PASS\n"
output+="***********************************\n"
output+="\n"
for t in ./$src_dir/*.cll; do
    output+="***********************************"
    output+="***********************************\n"
    to_stdout="TESTING FILE: ${t}"
    output+="${to_stdout}\n"
    output+="***********************************\n"

    output+="LL File\n\n"
    output+="$(./cll.native $t)\n"
    output+="***********************************\n"

    output+="File Contents\n\n"
    output+="$(cat $t)\n"
    output+="***********************************\n"

    output+="Program Output\n\n"

    filename=${t%.cll}
    filename=${filename##*/}
    output+="$(./bin/make_test_exe.sh $filename)"

    if test -f "./${build_dir}/${filename}"; then
        if [ "${filename}" = "stdin" ]; then
            progoutput="$(cat ${src_dir}/test_input | ./${build_dir}/${filename})"
        else
            if [ "${filename}" = "example_program" ]; then
                progoutput="$(cat ${src_dir}/example_program_input | ./${build_dir}/${filename})"
            else
                progoutput="$(./${build_dir}/${filename})"
            fi
        fi
        outputfile="${build_dir}/${filename}.out"
        echo $progoutput > $outputfile
        outputfilecheck="${src_dir}/${filename}.out"
        if cmp -s $outputfile $outputfilecheck; then
            echo "${green}${to_stdout} passed${reset}"
        else
            echo "${red}!!!${to_stdout} output failed!!!${reset}"
        fi
        output+="${fileoutput}\n"
    else
        echo "!!!${to_stdout} make_test_exe failed!!!"
    fi

    output+="***********************************"
    output+="***********************************\n"
    output+="\n"
done

#output+="***********************************\n"
#output+="THESE TESTS SHOULD FAIL\n"
#output+="***********************************\n"
#output+="\n"
#for t in ./test/fail/*.cll; do
#    output+="***********************************\n"
#    to_stdout="TESTING FILE: ${t}"
#    echo "$to_stdout"
#    output+="${to_stdout}\n"
#    output+="***********************************\n"
#    output+="$(./cll.native -s $t)\n"
#    output+="***********************************\n"
#    output+="\n"
#done
echo $output > test.out
