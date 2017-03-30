#!/bin/bash

SRC=`mktemp -d 2>/dev/null || mktemp -d -t 'src'`

cd ${SRC}
git clone --depth 1 https://github.com/facebook/rocksdb rocksdb

cd rocksdb
make static_lib
sudo make install
