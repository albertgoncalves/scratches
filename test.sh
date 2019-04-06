#!/usr/bin/env bash

set -e

runghc src/NaiveBayes.hs
ghc test/Test.hs src/NaiveBayes.hs -o test/Test
test/Test
