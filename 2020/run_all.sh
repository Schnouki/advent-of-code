#!/usr/bin/env sh

for f in day*.py; do
    python3 $f $@
    echo
done
