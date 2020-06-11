#!/bin/sh
if [ -x $1 ]; then
    exe=$1
    shift
    ag -g '.(hs|y|x)$' | entr -c ./$exe $*
else
    ag -g '.(hs|y|x)$' | entr -c $*
fi
