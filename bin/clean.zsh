#!/bin/zsh

setopt +o nomatch
setopt extendedglob
rm -rf **/*.out **/*.ast **/*.err
rm -rf **/*.o
rm -f parser.^mly
rm cll_build/*
unsetopt extendedglob
setopt -o nomatch

