#!/bin/zsh

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
    output+="$(./bin/make_exe.zsh $filename)"

    if test -f "./${build_dir}/${filename}"; then
        progoutput="$(./${build_dir}/${filename})"
        outputfile="${build_dir}/${filename}.out"
        echo $progoutput > $outputfile
        outputfilecheck="${src_dir}/${filename}.out"
        if cmp -s $outputfile $outputfilecheck; then
            echo "${to_stdout} passed"
        else
            echo "!!!${to_stdout} output failed!!!"
        fi
        output+="${fileoutput}\n"
    else
        echo "!!!${to_stdout} make_exe failed!!!"
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
