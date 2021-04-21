#!/bin/zsh

setopt +o nomatch
setopt extendedglob
rm -rf **/*.out **/*.ast **/*.err
rm -f parser.^mly
unsetopt extendedglob
setopt -o nomatch

rm cll_build/*
