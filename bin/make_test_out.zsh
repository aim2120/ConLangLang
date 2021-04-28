#!/bin/zsh

for t in ./test/*.cll; do
    filename=${t%.cll}
    filename=${filename##*/}
    ./bin/make_exe.zsh $filename
    output="$(./cll_build/$filename)"
    echo "**********************************"
    echo "MAKING OUTPUT FOR ${filename}"
    echo $output
    outfile="test/${filename}.out"
    echo $output > $outfile
done
