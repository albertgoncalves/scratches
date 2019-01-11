#!/usr/bin/env bash

set -e

ext=".tar.bz2"
url="https://spamassassin.apache.org/old/publiccorpus/"
stem="20021010_"
dir=data/

for handle in easy_ham hard_ham spam; do
    file=$stem$handle$ext
    filepath=$dir$file
    if [ ! -e $filepath ]; then
        wget $url$file -P $dir
    fi

    if [ ! -e $dir$handle ]; then
        tar xf $filepath -C $dir
    fi
done
