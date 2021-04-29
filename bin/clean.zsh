#!/bin/zsh

setopt +o nomatch
setopt extendedglob
rm "test.out"
rm -rf **/*.ast **/*.err
rm -rf **/*.o
rm -f parser.^mly
rm cll_build/*
unsetopt extendedglob
setopt -o nomatch

