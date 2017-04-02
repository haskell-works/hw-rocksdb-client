#!/bin/bash

cd ~

_version=5.2.1

if [ ! -d "rocksdb" ]; then
  git clone https://github.com/facebook/rocksdb rocksdb
fi

cd rocksdb

if ! ls librocksdb.${_version}.* 1> /dev/null 2>&1; then
  git pull
  git reset --hard v${_version}
  make static_lib
  make shared_lib
fi

sudo make install
