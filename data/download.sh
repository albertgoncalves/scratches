#!/usr/bin/env bash

set -e

ext=".tar.bz2"
url="https://spamassassin.apache.org/old/publiccorpus/"
stem="20021010_"

for handle in easy_ham hard_ham spam; do
    file=$stem$handle$ext
    filepath=$file
    if [ ! -e $filepath ]; then
        wget $url$file -P ./
    fi
    if [ ! -e $handle ]; then
        tar xf $filepath -C ./
    fi
done
