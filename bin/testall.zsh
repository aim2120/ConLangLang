#!/bin/zsh

touch test.out
output="***********************************\n"
output+="THESE TESTS SHOULD PASS\n"
output+="***********************************\n"
output+="\n"
for t in ./test/*.cll; do
    output+="***********************************"
    output+="***********************************\n"
    to_stdout="TESTING FILE: ${t}"
    echo "$to_stdout"
    output+="${to_stdout}\n"
    output+="***********************************\n"
    output+="File Contents\n\n"
    output+="$(cat $t)\n"
    output+="***********************************\n"
    output+="LL File\n\n"
    output+="$(./cll.native $t)\n"
    output+="***********************************\n"
    output+="Program Output\n\n"
    filename=${t%.cll}
    filename=${filename##*/}
    output+="$(./bin/make_exe.zsh $filename)"
    output+="$(./cll_build/$filename)\n"
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
