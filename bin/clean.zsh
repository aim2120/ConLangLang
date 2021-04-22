#!/bin/zsh

setopt +o nomatch
setopt extendedglob
rm -rf **/*.out **/*.ast **/*.err
rm -f parser.^mly
rm cll_build/*
unsetopt extendedglob
setopt -o nomatch

