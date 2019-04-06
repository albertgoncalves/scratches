#!/usr/bin/env bash

ghc test/Test.hs src/NaiveBayes.hs -o test/Test
if (( $? == 0 )); then
    test/Test
fi
